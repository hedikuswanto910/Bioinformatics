# =============================================================================
# 30_claude.R - Integrasi Claude (Anthropic API) untuk validasi & draf dokumen
# -----------------------------------------------------------------------------
# PENTING:
#  * Ini memakai API Key dari console.anthropic.com (tagihan terpisah, bayar
#    per pemakaian). Langganan Claude.ai (Pro/Team) TIDAK bisa disambungkan ke
#    aplikasi pihak ketiga.
#  * Simpan kunci di file .Renviron:  ANTHROPIC_API_KEY=sk-ant-...
#    Jangan pernah menulis kunci di kode atau meng-commit-nya ke GitHub.
#  * Isi narasi & PDF yang dikirim akan diproses oleh Anthropic. Jangan kirim
#    dokumen berisi data pribadi sensitif (NIK, data kesehatan, LHKPN rinci).
#  * Hasil Claude adalah SARAN pra-penilaian, bukan nilai resmi evaluator.
# =============================================================================

claude_tersedia <- function() nzchar(Sys.getenv("ANTHROPIC_API_KEY"))

# Kirim satu permintaan ke Messages API.
#   konten : list blok konten (teks / dokumen PDF)
#   skema  : JSON schema (list R) untuk keluaran terstruktur; NULL = teks bebas
# Mengembalikan list(ok, teks, data, model, token_in, token_out, biaya, pesan)
panggil_claude <- function(system, konten, skema = NULL, max_tokens = 16000,
                           effort = KONFIG$effort, model = KONFIG$model) {
  if (!claude_tersedia()) {
    return(list(ok = FALSE, pesan = "ANTHROPIC_API_KEY belum diatur. Lihat README bagian 'Menghubungkan Claude'."))
  }
  oc <- list(effort = effort)
  if (!is.null(skema)) oc$format <- list(type = "json_schema", schema = skema)
  body <- list(
    model = model, max_tokens = max_tokens, system = system,
    messages = list(list(role = "user", content = konten)),
    output_config = oc,
    # Bila filter keamanan menolak permintaan, server mencoba model cadangan
    # yang direkomendasikan Anthropic secara otomatis.
    fallbacks = "default"
  )
  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version` = KONFIG$api_versi,
      `anthropic-beta` = "server-side-fallback-2026-07-01",
      `content-type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(600) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 529)) |>
    httr2::req_error(is_error = function(r) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) return(list(ok = FALSE, pesan = paste("Gagal terhubung ke API:", conditionMessage(resp))))

  status <- httr2::resp_status(resp)
  isi <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
  if (status != 200) {
    pesan_api <- isi$error$message %||% httr2::resp_status_desc(resp)
    saran <- switch(as.character(status),
      "401" = "API key tidak valid atau sudah dicabut.",
      "403" = "API key tidak punya izin untuk model ini.",
      "413" = "Permintaan terlalu besar; kurangi jumlah/ukuran PDF.",
      "429" = "Batas pemakaian tercapai; coba lagi beberapa saat lagi.",
      "529" = "Server Anthropic sedang sibuk; coba lagi nanti.",
      "")
    return(list(ok = FALSE, pesan = sprintf("API error %d: %s %s", status, pesan_api, saran)))
  }

  hasil <- list(ok = TRUE, model = isi$model %||% model,
                token_in = isi$usage$input_tokens %||% 0, token_out = isi$usage$output_tokens %||% 0)
  hasil$biaya <- (hasil$token_in * KONFIG$harga_input + hasil$token_out * KONFIG$harga_output) / 1e6

  # Selalu periksa stop_reason sebelum membaca isi
  alasan <- isi$stop_reason %||% ""
  if (identical(alasan, "refusal")) {
    return(modifyList(hasil, list(ok = FALSE, pesan = "Permintaan ditolak oleh filter keamanan model. Ubah isi permintaan lalu coba lagi.")))
  }
  teks <- paste(vapply(Filter(function(b) identical(b$type, "text"), isi$content),
                       function(b) b$text %||% "", character(1)), collapse = "")
  if (identical(alasan, "max_tokens")) {
    return(modifyList(hasil, list(ok = FALSE, teks = teks, pesan = "Jawaban terpotong (batas token). Kurangi PDF atau naikkan max_tokens.")))
  }
  hasil$teks <- teks
  if (!is.null(skema)) {
    hasil$data <- tryCatch(jsonlite::fromJSON(teks, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(hasil$data)) return(modifyList(hasil, list(ok = FALSE, pesan = "Keluaran Claude bukan JSON yang valid.")))
  }
  hasil
}

# Ubah file PDF menjadi blok dokumen untuk API
blok_pdf <- function(path, judul = basename(path)) {
  data <- gsub("[\r\n]", "", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))
  list(type = "document", title = judul,
       source = list(type = "base64", media_type = "application/pdf", data = data))
}

# Kumpulan PDF unggahan untuk satu butir, dibatasi total ukuran
kumpulkan_pdf <- function(paths) {
  paths <- paths[file.exists(paths) & grepl("\\.pdf$", paths, ignore.case = TRUE)]
  if (!length(paths)) return(list(blok = list(), dilewati = character(0)))
  ukuran <- cumsum(file.info(paths)$size) / 1024^2
  pakai <- paths[ukuran <= KONFIG$maks_mb_pdf]
  list(blok = lapply(pakai, blok_pdf), dilewati = setdiff(paths, pakai))
}

.arr <- function(...) I(c(...))   # paksa jadi array JSON walau 1 elemen

# -----------------------------------------------------------------------------
# Konteks butir untuk prompt
# -----------------------------------------------------------------------------
konteks_butir <- function(b, isi, rinci = NULL, isi_rinci = NULL, rw = NULL, temuan = NULL) {
  krit <- kriteria_per_pilihan(b$penjelasan, b$pilihan)
  krit_teks <- if (length(krit)) paste(sprintf("  %s: %s", names(krit), krit), collapse = "\n") else b$penjelasan
  rinci_teks <- if (!is.null(rinci) && nrow(rinci)) {
    paste(sprintf("  - %s: %s", rinci$pertanyaan,
                  vapply(rinci$kode, function(k) if (k %in% names(isi_rinci)) isi_rinci[[k]] else "(kosong)", character(1))),
          collapse = "\n")
  } else ""
  rw_teks <- if (!is.null(rw) && nrow(rw) == 1) {
    sprintf("Penilaian sebelumnya: unit=%s, TP PTN=%s, TPSK=%s, TPI=%s\nCatatan TP PTN: %s\nCatatan TPSK: %s\nCatatan TPI: %s",
            rw$jawaban_unit, rw$jawaban_tp, rw$jawaban_tpsk, rw$jawaban_tpi,
            rw$catatan_tp %||% "-", rw$catatan_tpsk %||% "-", rw$catatan_tpi %||% "-")
  } else "Penilaian sebelumnya: tidak ada data."
  tm_teks <- if (!is.null(temuan) && nrow(temuan)) paste(sprintf("  - [%s] %s", temuan$tingkat, temuan$pesan), collapse = "\n") else "  (tidak ada)"
  paste0(
    "<butir>\n",
    "Kode: ", b$kode, "\nArea: ", b$area, " > ", b$komponen, " > ", b$sub_nama, "\n",
    "Pertanyaan: ", b$pertanyaan, "\n",
    "Jenis jawaban: ", b$pilihan, "\nKriteria tiap pilihan:\n", krit_teks, "\n",
    "</butir>\n\n<isian_unit>\n",
    "Jawaban yang diklaim: ", isi$jawaban %||% "(kosong)", "\n",
    if (nzchar(rinci_teks)) paste0("Rincian:\n", rinci_teks, "\n") else "",
    "Narasi/keterangan:\n", isi$narasi %||% "(kosong)", "\n",
    "Daftar bukti (judul | tautan):\n", isi$bukti %||% "(kosong)", "\n",
    "</isian_unit>\n\n<riwayat>\n", rw_teks, "\n</riwayat>\n\n",
    "<temuan_validasi_otomatis>\n", tm_teks, "\n</temuan_validasi_otomatis>\n")
}

SYSTEM_VALIDASI <- paste(
  "Anda membantu Tim Zona Integritas sebuah unit kerja di perguruan tinggi negeri Indonesia",
  "menyiapkan Lembar Kerja Evaluasi (LKE) Zona Integritas menuju WBK/WBBM sebelum dinilai",
  "oleh Tim Penilai (TP PTN, TPSK, dan TPI). Tugas Anda adalah pra-penilaian yang jujur dan",
  "ketat, seperti evaluator yang skeptis: tentukan jawaban yang benar-benar didukung bukti,",
  "bukan jawaban yang diharapkan unit.",
  "",
  "Prinsip penilaian:",
  "- Jawaban tertinggi hanya layak bila SETIAP unsur kriterianya terbukti (mis. 'seluruh',",
  "  'berkala', 'melibatkan pimpinan', 'ditindaklanjuti', 'inovasi', 'dampak terukur').",
  "- Bukti yang hanya berupa judul tanpa isi tidak bisa diverifikasi; nyatakan hal itu dan",
  "  nilai berdasarkan apa yang bisa diverifikasi. Bila PDF dilampirkan, baca isinya.",
  "- Siklus lengkap biasanya dituntut: dasar/penetapan (SK, pedoman) -> pelaksanaan",
  "  (undangan, daftar hadir, notulen, foto, laporan) -> monitoring & evaluasi -> tindak lanjut.",
  "- Bukti harus dari periode penilaian berjalan.",
  "- Manfaatkan catatan evaluator periode sebelumnya: apakah kekurangan yang mereka sebut",
  "  sudah ditutup oleh isian sekarang?",
  "- Jangan mengarang fakta, nomor SK, tanggal, atau nama dokumen yang tidak ada di isian.",
  "Tulis semua keluaran dalam Bahasa Indonesia yang ringkas dan operasional.",
  sep = "\n")

SKEMA_VALIDASI <- list(
  type = "object", additionalProperties = FALSE,
  required = .arr("jawaban_disarankan", "tingkat_risiko", "alasan", "unsur_kriteria",
                  "dokumen_perlu_ditambah", "perbaikan_narasi"),
  properties = list(
    jawaban_disarankan = list(type = "string",
                              description = "Jawaban yang menurut bukti saat ini paling dapat dipertahankan (mis. A, B, Ya, Tidak, atau angka)."),
    tingkat_risiko = list(type = "string", enum = .arr("rendah", "sedang", "tinggi"),
                          description = "Risiko nilai diturunkan evaluator bila diunggah apa adanya."),
    alasan = list(type = "string"),
    unsur_kriteria = list(type = "array", items = list(
      type = "object", additionalProperties = FALSE,
      required = .arr("unsur", "terbukti", "keterangan"),
      properties = list(unsur = list(type = "string"), terbukti = list(type = "boolean"),
                        keterangan = list(type = "string")))),
    dokumen_perlu_ditambah = list(type = "array", items = list(type = "string")),
    perbaikan_narasi = list(type = "string", description = "Saran konkret memperbaiki narasi; bukan narasi lengkap.")
  )
)

claude_validasi_butir <- function(b, isi, rinci = NULL, isi_rinci = NULL, rw = NULL, temuan = NULL, pdf = character(0)) {
  dok <- kumpulkan_pdf(pdf)
  teks <- paste0(konteks_butir(b, isi, rinci, isi_rinci, rw, temuan),
                 if (length(dok$blok)) sprintf("\n%d PDF bukti dilampirkan di atas; nilai berdasarkan isinya.\n", length(dok$blok))
                 else "\nTidak ada PDF yang dilampirkan; hanya judul bukti yang tersedia.\n",
                 "\nLakukan pra-penilaian butir ini.")
  h <- panggil_claude(SYSTEM_VALIDASI, c(dok$blok, list(list(type = "text", text = teks))), SKEMA_VALIDASI)
  h$pdf_dilewati <- dok$dilewati
  h
}

# -----------------------------------------------------------------------------
# Draf narasi (kolom Keterangan) dari isian yang ADA - tanpa mengarang
# -----------------------------------------------------------------------------
SYSTEM_NARASI <- paste(
  "Anda menyusun draf narasi kolom 'Keterangan' LKE Zona Integritas untuk unit kerja.",
  "Narasi yang baik menjawab kriteria secara langsung: apa yang dilakukan, dasar penetapannya,",
  "kapan dan seberapa sering, siapa yang terlibat (termasuk pimpinan), hasil/dampaknya, monitoring",
  "dan tindak lanjutnya; lalu merujuk dokumen bukti dengan nomor urut [1], [2], dst. sesuai daftar bukti.",
  "Gunakan HANYA fakta yang ada di isian unit. Bila ada unsur kriteria yang belum didukung data,",
  "tulis penanda [PERLU DIISI: ...] di tempatnya dan cantumkan di daftar data yang perlu dilengkapi.",
  "Jangan menyatakan sesuatu lebih tinggi dari jawaban yang didukung bukti. Bahasa Indonesia formal,",
  "3-5 paragraf ringkas, tanpa basa-basi pembuka.",
  sep = "\n")

SKEMA_NARASI <- list(
  type = "object", additionalProperties = FALSE,
  required = .arr("narasi", "data_perlu_dilengkapi"),
  properties = list(narasi = list(type = "string"),
                    data_perlu_dilengkapi = list(type = "array", items = list(type = "string")))
)

claude_draf_narasi <- function(b, isi, rinci = NULL, isi_rinci = NULL, rw = NULL, pdf = character(0)) {
  dok <- kumpulkan_pdf(pdf)
  teks <- paste0(konteks_butir(b, isi, rinci, isi_rinci, rw, NULL),
                 "\nSusun draf narasi kolom Keterangan untuk butir ini.")
  panggil_claude(SYSTEM_NARASI, c(dok$blok, list(list(type = "text", text = teks))), SKEMA_NARASI)
}

# -----------------------------------------------------------------------------
# Draf dokumen pendukung dari fakta kegiatan yang SUDAH terjadi
# -----------------------------------------------------------------------------
JENIS_DOKUMEN <- c("Laporan Monitoring dan Evaluasi", "Laporan Tindak Lanjut Hasil Monev",
                   "Notulen Rapat", "Rencana Aksi / Rencana Kerja", "Laporan Kegiatan Sosialisasi",
                   "Laporan Evaluasi SOP")

SYSTEM_DOKUMEN <- paste(
  "Anda membantu menyusun draf dokumen administrasi perguruan tinggi negeri Indonesia untuk",
  "kegiatan yang SUDAH dilaksanakan, berdasarkan fakta yang diberikan pengguna.",
  "Larangan mutlak: jangan mengarang kegiatan, tanggal, peserta, angka, temuan, nomor surat, atau",
  "tanda tangan. Setiap informasi yang dibutuhkan struktur dokumen tetapi tidak ada di fakta",
  "ditulis sebagai [PERLU DIISI: ...]. Dokumen ini akan menjadi bukti dalam penilaian integritas;",
  "dokumen yang tidak sesuai kenyataan adalah pemalsuan.",
  "Keluarkan dokumen dalam Markdown sederhana: judul (#), subjudul (##), paragraf, daftar (-),",
  "dan tabel Markdown bila perlu.",
  sep = "\n")

claude_draf_dokumen <- function(jenis, fakta, b = NULL) {
  konteks <- if (!is.null(b)) sprintf("Dokumen ini untuk mendukung butir LKE %s: %s\nKriteria: %s\n\n", b$kode, b$pertanyaan, b$penjelasan) else ""
  teks <- paste0(konteks, "Jenis dokumen: ", jenis, "\nUnit kerja: ", KONFIG$unit,
                 "\n\n<fakta_kegiatan>\n", fakta, "\n</fakta_kegiatan>\n\nSusun draf dokumennya.")
  panggil_claude(SYSTEM_DOKUMEN, list(list(type = "text", text = teks)), NULL, max_tokens = 16000)
}
