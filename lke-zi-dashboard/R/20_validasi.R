# =============================================================================
# 20_validasi.R - Validasi isian LKE berbasis aturan (tanpa AI)
# -----------------------------------------------------------------------------
# Aturan disusun dari pola catatan evaluator (TP PTN, TPSK, TPI) yang paling
# sering menurunkan nilai:
#   - narasi/keterangan kosong atau terlalu tipis
#   - bukti tidak ada, tautan rusak, atau bukan periode penilaian
#   - jawaban mengklaim kriteria (mis. "berkala", "inovasi", "seluruh",
#     "ditindaklanjuti") tanpa bukti/narasi yang menunjukkannya
#   - data jumlah/persentase tidak konsisten (#DIV/0!, rincian tidak cocok)
#   - butir yang tahun lalu diturunkan evaluator tetapi belum diperbaiki
#
# Tingkat temuan: "merah" (wajib diperbaiki), "kuning" (berisiko diturunkan),
# "info" (catatan/pengingat).
# =============================================================================

KONSEP <- list(
  inovasi       = list(label = "inovasi", pemicu = "inovasi",
                       bukti = c("inovasi")),
  berkala       = list(label = "dilakukan secara berkala", pemicu = "berkala|secara rutin|periodik",
                       bukti = c("berkala", "triwulan", "semester", "bulanan", "per bulan", "tahunan", "rutin", "periodik")),
  seluruh       = list(label = "mencakup seluruh/semua", pemicu = "\\b(seluruh|semua)\\b",
                       bukti = c("seluruh", "semua", "rekap", "100%", "daftar")),
  tindak_lanjut = list(label = "telah ditindaklanjuti", pemicu = "tindak ?lanjut|ditindaklanjuti",
                       bukti = c("tindak lanjut", "tindaklanjut", "ditindaklanjuti", "rencana aksi perbaikan")),
  monev         = list(label = "monitoring/evaluasi/reviu", pemicu = "monitoring|evaluasi|monev|reviu|direviu",
                       bukti = c("monev", "monitoring", "evaluasi", "reviu", "review")),
  pimpinan      = list(label = "melibatkan pimpinan", pemicu = "pimpinan",
                       bukti = c("pimpinan", "dekan", "wakil dekan", "kepala")),
  sosialisasi   = list(label = "sosialisasi/internalisasi", pemicu = "sosialisasi|internalisasi|dikomunikasikan|diinformasikan|dipublikasikan|publikasi",
                       bukti = c("sosialisasi", "internalisasi", "undangan", "daftar hadir", "publikasi", "media", "website", "instagram")),
  survei        = list(label = "survei", pemicu = "survei|survey",
                       bukti = c("survei", "survey", "ikm", "skm", "kuesioner", "kuisioner")),
  terintegrasi  = list(label = "terintegrasi/terpadu", pemicu = "terintegrasi|terpadu",
                       bukti = c("integrasi", "terintegrasi", "terpadu"))
)

# Buang frasa negatif ("tidak secara berkala", "belum ditindaklanjuti") agar
# tidak dianggap sebagai syarat kriteria.
.tanpa_negasi <- function(x) gsub("\\b(tidak|belum|tanpa)\\b[^,.;\\n]{0,40}", " ", tolower(x), perl = TRUE)

konsep_dalam_kriteria <- function(teks) {
  t <- .tanpa_negasi(teks)
  names(KONSEP)[vapply(KONSEP, function(k) grepl(k$pemicu, t, perl = TRUE), logical(1))]
}

# Pecah teks bukti ("Judul | tautan" per baris)
urai_bukti <- function(teks) {
  baris <- trimws(unlist(strsplit(gsub("\r", "", teks %||% ""), "\n")))
  baris <- baris[nzchar(baris)]
  if (length(baris) == 0) return(data.frame(judul = character(0), tautan = character(0)))
  bagian <- strsplit(baris, "|", fixed = TRUE)
  data.frame(
    judul  = trimws(vapply(bagian, function(b) b[1], character(1))),
    tautan = trimws(vapply(bagian, function(b) if (length(b) > 1) paste(b[-1], collapse = "|") else "", character(1))),
    stringsAsFactors = FALSE)
}

.temuan <- function(kode, tingkat, aturan, pesan) {
  data.frame(kode = kode, tingkat = tingkat, aturan = aturan, pesan = pesan, stringsAsFactors = FALSE)
}

.urut_nilai <- function(pilihan, j) nilai_pilihan(pilihan, j)

# -----------------------------------------------------------------------------
# Validasi satu butir
#   b        : satu baris rubrik (tipe butir)
#   rinci    : baris rubrik rincian milik butir (boleh 0 baris)
#   isi      : list(jawaban, narasi, bukti) untuk butir
#   isi_rinci: vektor bernama nilai rincian
#   rw       : satu baris riwayat (boleh NULL)
# -----------------------------------------------------------------------------
validasi_butir <- function(b, rinci, isi, isi_rinci = character(0), rw = NULL, tahun = KONFIG$tahun) {
  out <- list(); tambah <- function(t, a, p) out[[length(out) + 1]] <<- .temuan(b$kode, t, a, p)
  jawaban <- trimws(isi$jawaban %||% ""); narasi <- trimws(isi$narasi %||% ""); bukti <- urai_bukti(isi$bukti)

  # --- 1. Jawaban ------------------------------------------------------------
  if (b$pilihan %in% names(PETA_NILAI) && !(jawaban %in% pilihan_jawaban(b$pilihan))) {
    tambah("merah", "V01", sprintf("Jawaban belum ditetapkan (pilihan yang tersedia: %s).", b$pilihan))
  }
  if (b$pilihan == "Nilai (0-4)") {
    v <- ke_angka(jawaban)
    if (is.na(v)) tambah("merah", "V01", "Nilai hasil survei eksternal (skala 0-4) belum diisikan.")
    else if (v < 0 || v > 4) tambah("merah", "V02", "Nilai hasil survei berada di luar rentang skala 0-4.")
  }

  # --- 2. Rincian jumlah & persentase -----------------------------------------
  if (nrow(rinci) > 0) {
    v <- vapply(rinci$kode, function(k) ke_angka(if (k %in% names(isi_rinci)) isi_rinci[[k]] else ""), numeric(1))
    kosong <- rinci$pertanyaan[is.na(v)]
    if (length(kosong)) tambah("merah", "V02", paste0("Komponen rincian berikut belum diisikan: ", paste(kosong, collapse = "; "), "."))
    if (any(v < 0, na.rm = TRUE)) tambah("merah", "V02", "Komponen rincian tidak dapat bernilai negatif.")
    if (!any(is.na(v))) {
      h <- hitung_persen(b$rumus_persen %||% "rasio", v)
      if (nzchar(h$pesan) && is.na(h$nilai)) {
        tambah("merah", "V03", paste0(h$pesan, ". Tim ZI perlu menetapkan perlakuan atas kondisi ini dan mendokumentasikan dasar pertimbangannya pada narasi."))
      }
      if ((b$rumus_persen %||% "") == "rasio" && v[length(v)] > v[1] && v[1] > 0) {
        tambah("kuning", "V04", sprintf("Nilai '%s' (%s) melampaui '%s' (%s); persentase dibatasi pada 100%%. Konsistensi data sumber perlu diverifikasi.",
                                        rinci$pertanyaan[length(v)], v[length(v)], rinci$pertanyaan[1], v[1]))
      }
      if (nrow(rinci) >= 4 && grepl("harus melaporkan", tolower(rinci$pertanyaan[1]))) {
        tengah <- v[2:(length(v) - 1)]
        if (sum(tengah) != v[1]) {
          tambah("merah", "V05", sprintf("Jumlah wajib lapor (%s) tidak sama dengan agregat rinciannya (%s); terdapat inkonsistensi internal data.", v[1], sum(tengah)))
        }
      }
      if ((b$rumus_persen %||% "") == "penurunan" && length(v) >= 3 && v[3] > v[2]) {
        tambah("kuning", "V06", "Jumlah pelanggaran yang dikenai sanksi melampaui jumlah pelanggaran pada tahun berjalan; data perlu diverifikasi.")
      }
      if (grepl("pengaduan", tolower(b$pertanyaan)) && length(v) == 3 && v[2] + v[3] > v[1]) {
        tambah("kuning", "V06", "Jumlah pengaduan dalam proses dan selesai melampaui jumlah pengaduan yang wajib ditindaklanjuti; data perlu diverifikasi.")
      }
    }
  }

  # --- 3. Narasi ---------------------------------------------------------------
  if (!nzchar(narasi)) {
    tambah("merah", "V10", "Narasi belum disusun. Catatan evaluator terdahulu secara konsisten mensyaratkan uraian yang menautkan klaim dengan bukti dukung.")
  } else if (nchar(narasi) < ATURAN$narasi_min_karakter) {
    tambah("kuning", "V10", sprintf("Narasi belum memadai (%d karakter; ambang minimal %d). Uraikan kegiatan, dasar penetapan, waktu dan frekuensi, pihak yang terlibat, hasil, serta rujukan nomor bukti.",
                                    nchar(narasi), ATURAN$narasi_min_karakter))
  }

  # --- 4. Bukti ----------------------------------------------------------------
  if (nrow(bukti) == 0) {
    tambah("merah", "V11", "Belum tersedia dokumen bukti dukung.")
  } else {
    tanpa_link <- bukti$judul[!grepl("^https?://", bukti$tautan)]
    if (length(tanpa_link)) tambah("merah", "V12", paste0("Tautan bukti kosong atau tidak valid pada: ", paste(utils::head(tanpa_link, 5), collapse = "; "), "."))
    pendek <- bukti$judul[nchar(bukti$judul) < ATURAN$judul_bukti_min]
    if (length(pendek)) tambah("kuning", "V12", paste0("Judul dokumen kurang deskriptif: ", paste(pendek, collapse = "; "), ". Gunakan konvensi TAHUN_KODE_JENIS_URAIAN."))
    if (anyDuplicated(bukti$tautan[nzchar(bukti$tautan)])) tambah("info", "V14", "Terdapat tautan yang dirujuk lebih dari satu kali.")
    th <- as.integer(unlist(regmatches(bukti$judul, gregexpr("20[0-9]{2}", bukti$judul))))
    if (length(th) == 0) {
      tambah("kuning", "V13", "Tidak satu pun judul bukti mencantumkan tahun. Penilaian berfokus pada periode berjalan sehingga tahun dokumen perlu dinyatakan secara eksplisit.")
    } else if (!any(th %in% c(tahun, tahun - 1))) {
      tambah("kuning", "V13", sprintf("Seluruh bukti bertahun %s; belum terdapat bukti periode %d/%d.", paste(sort(unique(th)), collapse = ", "), tahun - 1, tahun))
    }
    # Kendali identifikasi dokumen (ISO 9001:2015 klausul 7.5.2)
    kn <- cek_kendali_nama(bukti$judul, b$kode)
    if (length(kn$sufiks)) tambah("kuning", "V15", paste0("Identifikasi dokumen belum terkendali (sufiks unduhan/salinan): ",
                                   paste(utils::head(kn$sufiks, 4), collapse = "; "), ". Gunakan satu versi final dengan nama baku."))
    if (isTRUE(kn$tanpa_kode)) tambah("info", "V16", sprintf("Disarankan mencantumkan kode butir pada nama dokumen, misalnya %d_%s_SK_Uraian, agar keterlacakan bukti terjaga.", tahun, b$kode))
    lama <- bukti$judul[vapply(regmatches(bukti$judul, gregexpr("20[0-9]{2}", bukti$judul)),
                               function(y) length(y) > 0 && max(as.integer(y)) <= tahun - 3, logical(1))]
    if (length(lama)) tambah("info", "V17", paste0("Dokumen historis (tiga tahun atau lebih sebelum periode penilaian): ",
                                                   paste(utils::head(lama, 3), collapse = "; "), ". Pastikan terdapat padanan periode berjalan."))
    # Daftar periksa siklus PDCA (bukan prediktor nilai)
    pd <- tahap_pdca(bukti$judul, narasi)
    if (!all(pd)) {
      kurang <- vapply(names(pd)[!pd], function(t) TAHAP_PDCA[[t]]$label, character(1))
      tambah("info", "V21", paste0("Daftar periksa siklus PDCA: tahap ", paste(sprintf("%s (%s)", names(pd)[!pd], tolower(kurang)), collapse = ", "),
                                   " belum tampak pada judul bukti maupun narasi."))
    }
  }

  # --- 5. Klaim kriteria vs isi narasi/bukti ----------------------------------
  if (b$pilihan %in% names(PETA_NILAI) && jawaban %in% pilihan_jawaban(b$pilihan) &&
      .urut_nilai(b$pilihan, jawaban) > 0) {
    krit <- kriteria_per_pilihan(b$penjelasan, b$pilihan)[[jawaban]]
    if (b$pilihan == "Ya/Tidak") krit <- paste(b$pertanyaan, krit)
    korpus <- tolower(paste(narasi, paste(bukti$judul, collapse = " ")))
    for (k in konsep_dalam_kriteria(krit)) {
      if (!any(vapply(KONSEP[[k]]$bukti, function(s) grepl(s, korpus, fixed = TRUE), logical(1)))) {
        tambah("kuning", "V20", sprintf("Kriteria jawaban '%s' mensyaratkan unsur \"%s\", namun unsur tersebut belum tercermin pada narasi maupun judul bukti.",
                                         jawaban, KONSEP[[k]]$label))
      }
    }
  }

  # --- 6. Pelajaran dari penilaian sebelumnya ---------------------------------
  if (!is.null(rw) && nrow(rw) == 1 && b$pilihan %in% names(PETA_NILAI)) {
    ev <- c(TPI = rw$jawaban_tpi, TPSK = rw$jawaban_tpsk, `TP PTN` = rw$jawaban_tp)
    ev <- ev[nzchar(ev %||% "") & !is.na(ev)]
    if (length(ev)) {
      siapa <- names(ev)[1]; j_ev <- ev[[1]]
      catatan <- switch(siapa, TPI = rw$catatan_tpi, TPSK = rw$catatan_tpsk, rw$catatan_tp)
      n_unit <- .urut_nilai(b$pilihan, rw$jawaban_unit); n_ev <- .urut_nilai(b$pilihan, j_ev)
      if (!is.na(n_unit) && !is.na(n_ev) && n_ev < n_unit) {
        tambah("info", "V30", sprintf("Penilaian terdahulu: klaim unit %s, nilai %s %s. Catatan evaluator: %s",
                                      rw$jawaban_unit, siapa, j_ev, substr(catatan %||% "-", 1, 400)))
        n_kini <- .urut_nilai(b$pilihan, jawaban)
        sama <- nzchar(narasi) && identical(gsub("\\s+", " ", narasi), gsub("\\s+", " ", trimws(rw$narasi %||% "")))
        if (!is.na(n_kini) && n_kini > n_ev && sama) {
          tambah("kuning", "V31", sprintf("Klaim %s diajukan kembali dengan narasi yang identik dengan penilaian terdahulu (dinilai %s oleh %s). Diperlukan bukti tambahan yang secara langsung menjawab catatan evaluator.",
                                          jawaban, j_ev, siapa))
        }
      }
    }
  }

  if (length(out)) do.call(rbind, out) else .temuan(character(0), character(0), character(0), character(0))
}

# -----------------------------------------------------------------------------
# Validasi seluruh LKE
#   isian   : data.frame kode, jawaban, narasi, bukti
#   riwayat : data.frame hasil baca_riwayat() (boleh NULL)
# -----------------------------------------------------------------------------
validasi_semua <- function(rubrik, isian, riwayat = NULL, tahun = KONFIG$tahun) {
  isian <- isian %||% data.frame(kode = character(0), jawaban = character(0), narasi = character(0), bukti = character(0))
  idx <- stats::setNames(seq_len(nrow(isian)), isian$kode)
  ambil <- function(k, kol) if (k %in% names(idx)) isian[[kol]][idx[[k]]] %||% "" else ""
  nilai_rinci <- stats::setNames(isian$jawaban, isian$kode)
  bt <- rubrik[rubrik$tipe == "butir", ]
  hasil <- lapply(seq_len(nrow(bt)), function(i) {
    b <- bt[i, ]
    rinci <- rubrik[rubrik$tipe == "rincian" & rubrik$induk %in% b$kode, ]
    rw <- if (!is.null(riwayat)) riwayat[riwayat$kode == b$kode, ][1, , drop = FALSE] else NULL
    if (!is.null(rw) && is.na(rw$kode)) rw <- NULL
    validasi_butir(b, rinci, list(jawaban = ambil(b$kode, "jawaban"), narasi = ambil(b$kode, "narasi"), bukti = ambil(b$kode, "bukti")),
                   nilai_rinci, rw, tahun)
  })
  temuan <- do.call(rbind, hasil)

  # Konsistensi antarbutir: rincian berlabel sama dalam satu sub-komponen
  rc <- rubrik[rubrik$tipe == "rincian", ]
  rc$label <- tolower(gsub("\\W+", " ", rc$pertanyaan)); rc$nilai <- vapply(rc$kode, function(k) ke_angka(ambil(k, "jawaban")), numeric(1))
  for (g in split(rc, paste(rc$sub_kode, rc$kelompok, rc$label))) {
    v <- g$nilai[!is.na(g$nilai)]
    if (nrow(g) > 1 && length(unique(v)) > 1) {
      temuan <- rbind(temuan, .temuan(g$induk[1], "merah", "V07",
                                      sprintf("'%s' diisi berbeda antarbutir (%s). Data harus konsisten.", g$pertanyaan[1], paste(v, collapse = " vs "))))
    }
  }
  temuan
}

# Ringkas status per butir: belum diisi / merah / kuning / hijau
status_butir <- function(rubrik, isian, temuan) {
  bt <- rubrik[rubrik$tipe == "butir", c("kode", "bagian", "area", "komponen", "sub_nama", "pertanyaan", "pilihan")]
  isi <- isian[isian$kode %in% bt$kode, ]
  terisi <- bt$kode %in% isi$kode[nzchar(trimws(isi$jawaban %||% "")) | nzchar(trimws(isi$narasi %||% ""))]
  rinci_terisi <- vapply(bt$kode, function(k) {
    ks <- rubrik$kode[rubrik$tipe == "rincian" & rubrik$induk %in% k]
    length(ks) > 0 && any(nzchar(isian$jawaban[isian$kode %in% ks]))
  }, logical(1))
  m <- vapply(bt$kode, function(k) sum(temuan$kode == k & temuan$tingkat == "merah"), integer(1))
  y <- vapply(bt$kode, function(k) sum(temuan$kode == k & temuan$tingkat == "kuning"), integer(1))
  bt$status <- ifelse(!(terisi | rinci_terisi), "belum diisi", ifelse(m > 0, "merah", ifelse(y > 0, "kuning", "hijau")))
  bt$jml_merah <- m; bt$jml_kuning <- y
  bt
}
