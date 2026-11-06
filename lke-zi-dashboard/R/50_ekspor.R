# =============================================================================
# 50_ekspor.R - Ekspor lembar isian (Excel) dan dokumen Word
# -----------------------------------------------------------------------------
# Catatan: aplikasi sengaja TIDAK menulis ulang file template Kementerian.
# Pustaka openxlsx merusak bagian gambar/printer di template itu saat disimpan
# ulang (file jadi minta "diperbaiki" oleh Excel). Sebagai gantinya dibuat
# workbook baru yang urutannya sama dengan LKE (kolom "Baris LKE" merujuk ke
# nomor baris di template) untuk disalin ke portal / template resmi.
# =============================================================================

ekspor_lembar_isian <- function(rubrik, isian, status, temuan, skor, keluaran, tahun = KONFIG$tahun) {
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

# Laporan validasi: ringkasan nilai + daftar temuan per butir + narasi
laporan_validasi_docx <- function(rubrik, isian, status, temuan, skor, keluaran, tahun = KONFIG$tahun) {
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, sprintf("Laporan Validasi LKE Zona Integritas %d", tahun), style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf("%s - dibuat %s", KONFIG$unit, format(Sys.time(), "%d-%m-%Y %H:%M")), style = "Normal")
  doc <- officer::body_add_par(doc, "Simulasi nilai berdasarkan jawaban yang diklaim (butir kosong dihitung 0)", style = "heading 2")
  rk <- skor$ringkas; rk$skor <- round(rk$skor, 2)
  doc <- officer::body_add_table(doc, rk, style = "table_template")
  doc <- officer::body_add_par(doc, "Status butir", style = "heading 2")
  doc <- officer::body_add_table(doc, as.data.frame(table(Status = status$status)), style = "table_template")
  doc <- officer::body_add_par(doc, "Temuan per butir (merah & kuning)", style = "heading 2")
  idx <- stats::setNames(seq_len(nrow(isian)), isian$kode)
  for (k in status$kode[status$status %in% c("merah", "kuning", "belum diisi")]) {
    s <- status[status$kode == k, ]
    doc <- officer::body_add_par(doc, sprintf("%s [%s] %s", k, toupper(s$status), s$pertanyaan), style = "heading 3")
    t <- temuan[temuan$kode == k & temuan$tingkat != "info", ]
    for (p in sprintf("\u2022 (%s) %s", t$tingkat, t$pesan)) doc <- officer::body_add_par(doc, p, style = "Normal")
    if (k %in% names(idx)) {
      j <- isian$jawaban[idx[[k]]]
      doc <- officer::body_add_par(doc, paste("Jawaban:", ifelse(nzchar(j %||% ""), j, "(kosong)")), style = "Normal")
    }
  }
  print(doc, target = keluaran)
  invisible(keluaran)
}
