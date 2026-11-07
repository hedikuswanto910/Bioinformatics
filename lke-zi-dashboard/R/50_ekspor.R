# =============================================================================
# 50_ekspor.R - Ekspor lembar isian (Excel) dan dokumen Word
# -----------------------------------------------------------------------------
# Catatan: aplikasi sengaja TIDAK menulis ulang file template Kementerian.
# Pustaka openxlsx merusak bagian gambar/printer di template itu saat disimpan
# ulang (file jadi minta "diperbaiki" oleh Excel). Sebagai gantinya dibuat
# workbook baru yang urutannya sama dengan LKE (kolom "Baris LKE" merujuk ke
# nomor baris di template) untuk disalin ke portal / template resmi.
# =============================================================================

ekspor_lembar_isian <- function(rubrik, isian, status, temuan, skor, keluaran, tahun = KONFIG$tahun, capa = NULL) {
  idx <- stats::setNames(seq_len(nrow(isian)), isian$kode)
  ambil <- function(k, kol) if (k %in% names(idx)) isian[[kol]][idx[[k]]] %||% "" else ""
  nb <- stats::setNames(skor$butir$nilai, skor$butir$kode)
  baris <- lapply(seq_len(nrow(rubrik)), function(i) {
    r <- rubrik[i, ]
    b <- urai_bukti(ambil(r$kode, "bukti"))
    data.frame(
      `Baris LKE` = r$baris_excel, Kode = r$kode, Area = r$area, Komponen = r$komponen,
      `Sub-komponen` = r$sub_nama,
      Pertanyaan = if (r$tipe == "rincian") paste0("   - ", r$pertanyaan) else r$pertanyaan,
      `Pilihan` = r$pilihan, Jawaban = ambil(r$kode, "jawaban"),
      Nilai = if (r$kode %in% names(nb)) round(nb[[r$kode]], 4) else NA,
      Status = if (r$kode %in% status$kode) status$status[status$kode == r$kode] else "",
      `Narasi / Keterangan` = if (r$tipe == "butir") ambil(r$kode, "narasi") else "",
      `Daftar Bukti` = if (nrow(b)) paste(sprintf("%d. %s", seq_len(nrow(b)), b$judul), collapse = "\n") else "",
      `Tautan Bukti` = if (nrow(b)) paste(sprintf("%d. %s", seq_len(nrow(b)), b$tautan), collapse = "\n") else "",
      PIC = ambil(r$kode, "pic"), check.names = FALSE, stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, baris)

  wb <- openxlsx::createWorkbook()
  gaya_kepala <- openxlsx::createStyle(fgFill = "#1F3864", fontColour = "#FFFFFF", textDecoration = "bold", wrapText = TRUE, valign = "center")
  gaya_bungkus <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  warna <- c(merah = "#F8D7DA", kuning = "#FFF3CD", hijau = "#D1E7DD", `belum diisi` = "#E2E3E5")

  openxlsx::addWorksheet(wb, "Isian LKE")
  openxlsx::writeData(wb, "Isian LKE", df, headerStyle = gaya_kepala)
  openxlsx::addStyle(wb, "Isian LKE", gaya_bungkus, rows = 2:(nrow(df) + 1), cols = seq_len(ncol(df)), gridExpand = TRUE)
  for (s in names(warna)) {
    rr <- which(df$Status == s) + 1
    if (length(rr)) openxlsx::addStyle(wb, "Isian LKE", openxlsx::createStyle(fgFill = warna[[s]], wrapText = TRUE, valign = "top"),
                                       rows = rr, cols = which(names(df) == "Status"), gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::setColWidths(wb, "Isian LKE", cols = seq_len(ncol(df)), widths = c(8, 12, 12, 18, 22, 50, 10, 10, 8, 10, 70, 45, 45, 15))
  openxlsx::freezePane(wb, "Isian LKE", firstRow = TRUE, firstCol = FALSE)

  openxlsx::addWorksheet(wb, "Temuan Validasi")
  tm <- merge(temuan, rubrik[, c("kode", "pertanyaan")], by = "kode", all.x = TRUE)
  tm <- tm[order(match(tm$tingkat, c("merah", "kuning", "info")), tm$kode), c("kode", "tingkat", "aturan", "pesan", "pertanyaan")]
  openxlsx::writeData(wb, "Temuan Validasi", tm, headerStyle = gaya_kepala)
  openxlsx::setColWidths(wb, "Temuan Validasi", cols = 1:5, widths = c(12, 9, 8, 90, 60))

  openxlsx::addWorksheet(wb, "Rekap Nilai")
  rk <- skor$ringkas; rk$skor <- round(rk$skor, 3)
  kp <- skor$komponen; kp$persen <- round(100 * kp$skor / kp$bobot_komponen, 1); kp$skor <- round(kp$skor, 3)
  openxlsx::writeData(wb, "Rekap Nilai", paste("Simulasi nilai klaim LKE", tahun, "-", KONFIG$unit))
  openxlsx::writeData(wb, "Rekap Nilai", rk, startRow = 3, headerStyle = gaya_kepala)
  openxlsx::writeData(wb, "Rekap Nilai", kp[, c("area", "komponen", "bobot_komponen", "skor", "persen")], startRow = 11, headerStyle = gaya_kepala)
  openxlsx::setColWidths(wb, "Rekap Nilai", cols = 1:5, widths = c(14, 45, 14, 10, 10))

  if (!is.null(capa) && nrow(capa)) {
    openxlsx::addWorksheet(wb, "Tindakan Korektif")
    cp <- capa[, c("id", "kode", "sumber", "temuan", "akar_masalah", "tindakan", "pic", "tenggat", "status", "bukti_penutupan", "diperbarui")]
    names(cp) <- c("No", "Kode", "Sumber", "Temuan", "Akar masalah", "Tindakan korektif", "PIC", "Tenggat", "Status", "Bukti penutupan", "Diperbarui")
    openxlsx::writeData(wb, "Tindakan Korektif", cp, headerStyle = gaya_kepala)
    openxlsx::addStyle(wb, "Tindakan Korektif", gaya_bungkus, rows = 2:(nrow(cp) + 1), cols = seq_len(ncol(cp)), gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Tindakan Korektif", cols = seq_len(ncol(cp)), widths = c(5, 10, 18, 50, 40, 40, 15, 11, 16, 35, 17))
  }
  openxlsx::saveWorkbook(wb, keluaran, overwrite = TRUE)
  invisible(keluaran)
}

# Ubah Markdown sederhana (keluaran Claude) menjadi paragraf Word
.md_ke_docx <- function(doc, md) {
  baris <- unlist(strsplit(gsub("\r", "", md %||% ""), "\n"))
  tabel <- character(0)
  siram_tabel <- function(doc) {
    if (length(tabel) < 2) return(doc)
    sel <- lapply(tabel[!grepl("^\\s*\\|?\\s*:?-{2,}", tabel)], function(x) trimws(strsplit(gsub("^\\||\\|$", "", trimws(x)), "|", fixed = TRUE)[[1]]))
    n <- max(lengths(sel)); sel <- lapply(sel, function(x) c(x, rep("", n - length(x))))
    df <- as.data.frame(do.call(rbind, sel[-1]), stringsAsFactors = FALSE); names(df) <- make.unique(sel[[1]])
    officer::body_add_table(doc, df, style = "table_template")
  }
  for (x in baris) {
    if (grepl("^\\s*\\|", x)) { tabel <- c(tabel, x); next }
    if (length(tabel)) { doc <- siram_tabel(doc); tabel <- character(0) }
    x <- gsub("\\*\\*(.+?)\\*\\*", "\\1", x)
    if (grepl("^# ", x)) doc <- officer::body_add_par(doc, sub("^# ", "", x), style = "heading 1")
    else if (grepl("^## ", x)) doc <- officer::body_add_par(doc, sub("^## ", "", x), style = "heading 2")
    else if (grepl("^###+ ", x)) doc <- officer::body_add_par(doc, sub("^###+ ", "", x), style = "heading 3")
    else if (grepl("^\\s*[-*] ", x)) doc <- officer::body_add_par(doc, paste0("\u2022 ", sub("^\\s*[-*] ", "", x)), style = "Normal")
    else if (nzchar(trimws(x))) doc <- officer::body_add_par(doc, x, style = "Normal")
  }
  if (length(tabel)) doc <- siram_tabel(doc)
  doc
}

simpan_dokumen_docx <- function(markdown, keluaran) {
  doc <- .md_ke_docx(officer::read_docx(), markdown)
  print(doc, target = keluaran)
  invisible(keluaran)
}

# -----------------------------------------------------------------------------
# Laporan pra-penilaian mandiri berstruktur akademis, dilengkapi blok
# pengendalian dokumen (ISO 9001:2015 klausul 7.5.2-7.5.3)
# -----------------------------------------------------------------------------
REKOMENDASI_ATURAN <- c(
  V01 = "Menetapkan jawaban pada seluruh butir yang belum dijawab.",
  V02 = "Melengkapi dan memverifikasi data kuantitatif (rincian dan nilai survei).",
  V03 = "Menetapkan perlakuan atas penyebut bernilai nol dan mendokumentasikan dasar pertimbangannya.",
  V05 = "Merekonsiliasi jumlah wajib lapor dengan rinciannya.",
  V06 = "Memverifikasi konsistensi logis data pelanggaran dan pengaduan.",
  V07 = "Menyeragamkan nilai rincian yang sama antarbutir.",
  V10 = "Menyusun narasi yang menautkan klaim dengan bukti (dasar penetapan, pelaksanaan, hasil, monev, tindak lanjut).",
  V11 = "Melengkapi dokumen bukti dukung.",
  V12 = "Memastikan setiap bukti memiliki tautan yang valid dan judul yang deskriptif.",
  V13 = "Mengutamakan bukti yang berasal dari periode penilaian berjalan.",
  V15 = "Menerapkan pengendalian identifikasi dokumen dengan konvensi TAHUN_KODE_JENIS_URAIAN dan satu versi final.",
  V20 = "Menyelaraskan klaim dengan bukti: menambahkan bukti atas unsur kriteria yang belum terbukti atau menurunkan klaim.",
  V31 = "Menjawab catatan evaluator terdahulu dengan bukti baru sebelum mengajukan kembali klaim yang sama.")

NAMA_ATURAN <- c(
  V01 = "jawaban belum ditetapkan", V02 = "data kuantitatif belum lengkap", V03 = "penyebut bernilai nol",
  V04 = "pembilang melampaui penyebut", V05 = "rincian wajib lapor tidak cocok", V06 = "inkonsistensi logis data",
  V07 = "rincian antarbutir tidak seragam", V10 = "narasi belum memadai", V11 = "bukti tidak tersedia",
  V12 = "tautan atau judul bukti bermasalah", V13 = "bukti bukan periode berjalan", V15 = "identifikasi dokumen tidak terkendali",
  V20 = "unsur kriteria tidak tercermin", V31 = "klaim berulang tanpa bukti baru")
.persen_id <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

.par <- function(doc, teks, gaya = "Normal") officer::body_add_par(doc, teks, style = gaya)
# Paragraf tebal tanpa penomoran otomatis (gaya Heading pada templat bawaan officer bernomor)
.tebal <- function(doc, teks, ukuran = 11, rata = "left") {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(teks, officer::fp_text(bold = TRUE, font.size = ukuran)),
                                            fp_p = officer::fp_par(text.align = rata, padding.top = 6, padding.bottom = 4)))
}
.tabel <- function(doc, df) officer::body_add_table(doc, df, style = "table_template")

laporan_validasi_docx <- function(rubrik, isian, status, temuan, skor, keluaran, tahun = KONFIG$tahun,
                                  capa = NULL, skenario = NULL, riwayat = NULL) {
  waktu <- Sys.time()
  n <- nrow(status); tab <- table(factor(status$status, c("hijau", "kuning", "merah", "belum diisi")))
  pct <- function(x) .persen_id(100 * x / max(n, 1))
  tm <- temuan[temuan$tingkat %in% c("merah", "kuning"), ]
  doc <- officer::read_docx()

  # Judul dan pengendalian dokumen
  doc <- .tebal(doc, sprintf("Laporan Pra-Penilaian Mandiri Lembar Kerja Evaluasi Zona Integritas Tahun %d", tahun), 16, "center")
  doc <- .tebal(doc, KONFIG$unit, 12, "center")
  doc <- .tebal(doc, "Pengendalian dokumen")
  doc <- .tabel(doc, data.frame(
    Unsur = c("Nomor dokumen", "Revisi", "Tanggal terbit", "Klasifikasi", "Disusun oleh", "Diperiksa oleh", "Disetujui oleh"),
    Keterangan = c(sprintf("LPM-ZI/%d/%s", tahun, format(waktu, "%Y%m%d-%H%M")), "00", format(waktu, "%d-%m-%Y"),
                   "Internal - terbatas", "Tim Pembangunan Zona Integritas", "[nama dan jabatan]", "[nama dan jabatan]")))

  # 1. Pendahuluan
  doc <- .par(doc, "Pendahuluan", "heading 1")
  doc <- .par(doc, paste(
    "Laporan ini menyajikan hasil pra-penilaian mandiri atas isian Lembar Kerja Evaluasi (LKE) Zona Integritas sebelum",
    "dinilai oleh Tim Penilai. Tujuannya adalah mengidentifikasi kesenjangan antara klaim jawaban dan bukti yang dapat",
    "diverifikasi, sehingga perbaikan dapat diprioritaskan secara sistematis. Ruang lingkup mencakup seluruh",
    sprintf("%d butir pada komponen Pengungkit (Pemenuhan dan Reform) serta komponen Hasil.", n)))

  # 2. Metode
  doc <- .par(doc, "Metode", "heading 1")
  doc <- .par(doc, paste(
    "Skor dihitung menggunakan rumus LKE: nilai butir kategorik (Ya/Tidak; A-E) dikonversi ke skala 0-1, nilai sub-komponen",
    "adalah rerata nilai butir dikali bobot, dan skor komponen merupakan penjumlahan sub-komponen. Berbeda dengan templat",
    "Excel yang mengabaikan butir kosong, laporan ini menghitung butir kosong sebagai nol (pendekatan konservatif)."))
  doc <- .par(doc, paste(
    "Validasi dilakukan berbasis aturan yang mencakup kelengkapan jawaban, konsistensi data kuantitatif, kecukupan narasi,",
    "keberadaan dan keterkinian bukti, pengendalian identifikasi dokumen, kesesuaian klaim dengan unsur kriteria, serta",
    "pembelajaran dari catatan evaluator terdahulu. Butir berstatus merah memuat sekurang-kurangnya satu ketidaksesuaian",
    "yang wajib diperbaiki; kuning memuat risiko penurunan nilai; hijau tidak memuat temuan merah maupun kuning."))
  doc <- .par(doc, paste(
    "Analisis sensitivitas menyajikan skenario konservatif (butir merah pada tingkat terendah, butir kuning diturunkan satu",
    "tingkat) dan, bila data tersedia, skenario rasio historis nilai TPI terhadap klaim unit."))

  # 3. Hasil
  doc <- .par(doc, "Hasil", "heading 1")
  doc <- .par(doc, "Skor klaim", "heading 2")
  rk <- skor$ringkas; rk$skor <- formatC(rk$skor, format = "f", digits = 2, decimal.mark = ","); names(rk) <- c("Komponen", "Bobot", "Skor klaim")
  doc <- .tabel(doc, rk)
  if (!is.null(skenario)) {
    doc <- .par(doc, "Analisis sensitivitas", "heading 2")
    f2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
    sk <- data.frame(Skenario = skenario$skenario, Pengungkit = f2(skenario$pengungkit), Hasil = f2(skenario$hasil),
                     Total = f2(skenario$pengungkit + skenario$hasil))
    doc <- .tabel(doc, sk)
  }
  doc <- .par(doc, "Distribusi status validasi", "heading 2")
  doc <- .tabel(doc, data.frame(Status = names(tab), Jumlah = as.integer(tab), Persentase = pct(as.integer(tab))))
  doc <- .par(doc, "Status validasi per komponen", "heading 2")
  st <- merge(status, unique(rubrik[rubrik$tipe == "butir", c("kode", "komponen_no")]), by = "kode")
  st$zona <- ifelse(st$bagian == "Hasil", "Hasil", paste0("Zona ", st$komponen_no, " (", substr(st$area, 1, 1), ")"))
  pz <- as.data.frame.matrix(table(st$zona, factor(st$status, c("hijau", "kuning", "merah", "belum diisi"))))
  doc <- .tabel(doc, cbind(Komponen = rownames(pz), pz))
  if (!is.null(capa) && nrow(capa)) {
    doc <- .par(doc, "Tindakan korektif", "heading 2")
    doc <- .tabel(doc, as.data.frame(table(Status = factor(capa$status, STATUS_CAPA)), responseName = "Jumlah"))
  }

  # 4. Pembahasan
  doc <- .par(doc, "Pembahasan", "heading 1")
  beban <- sort(tapply(st$status %in% c("merah", "kuning"), st$zona, sum), decreasing = TRUE)
  aturan_top <- sort(table(tm$aturan), decreasing = TRUE)
  doc <- .par(doc, sprintf(paste(
    "Sebanyak %d butir (%s) berstatus merah dan %d butir (%s) berstatus kuning, sedangkan %d butir (%s) memenuhi seluruh",
    "aturan validasi. Konsentrasi temuan tertinggi terdapat pada %s."),
    tab[["merah"]], pct(tab[["merah"]]), tab[["kuning"]], pct(tab[["kuning"]]), tab[["hijau"]], pct(tab[["hijau"]]),
    paste(sprintf("%s (%d butir)", names(beban)[seq_len(min(3, length(beban)))], beban[seq_len(min(3, length(beban)))]), collapse = ", ")))
  if (length(aturan_top)) doc <- .par(doc, sprintf(
    "Jenis temuan yang paling sering muncul adalah %s.",
    paste(sprintf("%s - %s (%d temuan)", names(aturan_top)[seq_len(min(3, length(aturan_top)))],
                  NAMA_ATURAN[names(aturan_top)[seq_len(min(3, length(aturan_top)))]],
                  aturan_top[seq_len(min(3, length(aturan_top)))]), collapse = "; ")))
  if (!is.null(riwayat) && any(nzchar(riwayat$jawaban_tpi))) doc <- .par(doc, paste(
    "Sebagai pembanding, pada penilaian terdahulu sebagian besar butir memperoleh nilai evaluator yang lebih rendah daripada",
    "klaim unit. Pola ini menegaskan bahwa risiko utama terletak pada kesenjangan antara klaim dan bukti yang dapat",
    "diverifikasi, bukan semata-mata pada kelengkapan administratif."))

  # 5. Keterbatasan
  doc <- .par(doc, "Keterbatasan", "heading 1")
  for (t in c(
    "Validasi berbasis aturan menilai bentuk dan kelengkapan isian, bukan substansi isi dokumen.",
    "Kategorisasi unsur kriteria dan tahap PDCA bergantung pada pencocokan kata kunci pada narasi dan judul dokumen.",
    "Analisis sensitivitas merupakan ilustrasi rentang risiko dan belum divalidasi pada data penilaian independen.",
    "Skor klaim tidak dapat ditafsirkan sebagai prediksi nilai Tim Penilai."))
    doc <- .par(doc, paste("\u2022", t))

  # 6. Simpulan dan rekomendasi
  doc <- .par(doc, "Simpulan dan rekomendasi", "heading 1")
  doc <- .par(doc, sprintf(paste(
    "Isian LKE belum sepenuhnya siap diajukan: %d butir masih memuat ketidaksesuaian kritis dan %d butir berisiko",
    "diturunkan. Rekomendasi berikut disusun berdasarkan jenis temuan yang dominan dan diurutkan menurut frekuensinya."),
    tab[["merah"]], tab[["kuning"]]))
  rek <- REKOMENDASI_ATURAN[intersect(names(aturan_top), names(REKOMENDASI_ATURAN))]
  for (i in seq_along(rek)) doc <- .par(doc, sprintf("%d. %s (%s)", i, rek[[i]], names(rek)[i]))
  doc <- .par(doc, sprintf("%d. Mencatat setiap ketidaksesuaian pada register tindakan korektif beserta akar masalah, penanggung jawab, tenggat, dan verifikasi efektivitas.", length(rek) + 1))

  # Lampiran
  doc <- officer::body_add_break(doc)
  doc <- .tebal(doc, "Lampiran A. Rincian temuan per butir", 13)
  idx <- stats::setNames(seq_len(nrow(isian)), isian$kode)
  for (k in status$kode[status$status %in% c("merah", "kuning", "belum diisi")]) {
    s <- status[status$kode == k, ]
    doc <- .tebal(doc, sprintf("%s [%s] %s", k, toupper(s$status), s$pertanyaan))
    t <- temuan[temuan$kode == k & temuan$tingkat != "info", ]
    for (p in sprintf("\u2022 [%s] %s", t$aturan, t$pesan)) doc <- .par(doc, p)
    if (k %in% names(idx)) {
      j <- isian$jawaban[idx[[k]]]
      doc <- .par(doc, paste("Jawaban yang diklaim:", ifelse(nzchar(j %||% ""), j, "(belum diisi)")))
    }
  }
  if (!is.null(capa) && nrow(capa)) {
    doc <- .tebal(doc, "Lampiran B. Register tindakan korektif", 13)
    doc <- .tabel(doc, data.frame(No = capa$id, Kode = capa$kode, Temuan = substr(capa$temuan, 1, 150),
                                  `Tindakan` = substr(capa$tindakan, 1, 150), PIC = capa$pic, Tenggat = capa$tenggat,
                                  Status = capa$status, check.names = FALSE))
  }
  print(doc, target = keluaran)
  invisible(keluaran)
}
