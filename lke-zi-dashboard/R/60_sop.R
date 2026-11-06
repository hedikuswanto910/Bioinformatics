# =============================================================================
# 60_sop.R - Pemeriksaan daftar Prosedur Mutu (SOP) dan file PDF-nya
# -----------------------------------------------------------------------------
# Mendukung butir LKE "SOP telah diterapkan / dievaluasi" (Penataan Tatalaksana).
# Pemeriksaan otomatis yang mencerminkan catatan admin SInERGI:
#   - Nomor dokumen ganda / nama file tidak cocok dengan nomor dokumen
#   - Versi di daftar != nomor revisi di PDF != versi di nama file
#   - "Halaman x dari N" di kop tidak sesuai jumlah halaman sebenarnya
#   - Status "Belum Upload" / "Revisi" / "Review Kembali"
#   - Dokumen di daftar tanpa PDF, atau PDF tanpa entri di daftar
# =============================================================================

# Baca sheet daftar PM (kolom: No, Judul, Versi, No. Dokumen Fakultas,
# No. Sistem Dokumen, Tanggal Terbit, Status, KET, Nama File)
baca_daftar_sop <- function(path, sheet = 1) {
  d <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_types = "text"))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- c("no", "judul", "versi", "no_dokumen", "no_sistem", "tanggal", "status", "bidang", "nama_file")[seq_len(ncol(d))]
  d <- d[!is.na(d$judul) & nzchar(d$judul), ]
  d$no_dokumen <- trimws(d$no_dokumen)
  d
}

.kode_dok <- function(x) {
  # "PM/UNHAS/FK/KMTR/03" -> "KMTR-3" ; "KMTR PM 03" -> "KMTR-3"
  x <- toupper(x %||% "")
  m <- regmatches(x, regexec("(AKDM|KMTR|KMHS|PSDM)[_/ ]*(PM[_/ ]*)?0*([0-9]+)", x))
  vapply(m, function(z) if (length(z) == 4) paste0(z[2], "-", as.integer(z[4])) else NA_character_, character(1))
}

# Periksa satu PDF: nomor dokumen, nomor revisi, dan konsistensi "Halaman x dari N"
periksa_pdf_sop <- function(path, nama = basename(path)) {
  teks <- tryCatch(pdftools::pdf_text(path), error = function(e) NULL)
  if (is.null(teks)) return(list(nama = nama, halaman = NA, no_dok = NA, revisi = NA, masalah = "PDF tidak dapat dibaca"))
  n <- length(teks); semua <- paste(teks, collapse = "\n")
  no_dok <- regmatches(semua, regexpr("PM/UNHAS/FK/[A-Z]+/[0-9]+", semua))[1] %||% NA
  rev <- regmatches(semua, regexec("(Nomor Revisi|No\\. Revisi)\\s*:\\s*([0-9]+)", semua))[[1]]
  rev <- if (length(rev) == 3) as.integer(rev[3]) else NA_integer_
  masalah <- character(0)
  hal <- regmatches(teks, regexec("Halaman\\s*([0-9]+)\\s*dari\\s*([0-9]+)", teks))
  ada <- which(lengths(hal) == 3)
  if (length(ada)) {
    ke <- vapply(hal[ada], function(z) as.integer(z[2]), integer(1))
    dari <- vapply(hal[ada], function(z) as.integer(z[3]), integer(1))
    if (any(dari != n)) masalah <- c(masalah, sprintf("Kop menulis 'dari %s' padahal PDF %d halaman", paste(unique(dari[dari != n]), collapse = "/"), n))
    salah <- ada[ke != ada]
    if (length(salah)) masalah <- c(masalah, sprintf("Nomor halaman di kop tidak urut pada halaman %s", paste(utils::head(salah, 5), collapse = ", ")))
  } else {
    masalah <- c(masalah, "Kop 'Halaman x dari N' tidak ditemukan")
  }
  if (is.na(no_dok)) masalah <- c(masalah, "Nomor dokumen PM/UNHAS/FK/... tidak ditemukan di PDF")
  if (is.na(rev)) masalah <- c(masalah, "Nomor revisi tidak ditemukan di PDF")
  list(nama = nama, halaman = n, no_dok = no_dok, revisi = rev, masalah = paste(masalah, collapse = "; "))
}

# Gabungkan daftar + hasil PDF menjadi tabel temuan
#   pdf_paths : vektor path file PDF; pdf_nama: nama asli file (untuk unggahan Shiny)
periksa_sop <- function(daftar, pdf_paths = character(0), pdf_nama = basename(pdf_paths)) {
  hasil_pdf <- lapply(seq_along(pdf_paths), function(i) periksa_pdf_sop(pdf_paths[i], pdf_nama[i]))
  pdf <- if (length(hasil_pdf)) do.call(rbind, lapply(hasil_pdf, as.data.frame, stringsAsFactors = FALSE)) else
    data.frame(nama = character(0), halaman = integer(0), no_dok = character(0), revisi = integer(0), masalah = character(0))
  pdf$kode <- .kode_dok(ifelse(is.na(pdf$no_dok), pdf$nama, pdf$no_dok))
  pdf$kode_nama_file <- .kode_dok(pdf$nama)
  pdf$versi_nama_file <- suppressWarnings(as.numeric(sub(".*v([0-9]+(\\.[0-9]+)?).*", "\\1", pdf$nama)))

  daftar$kode <- .kode_dok(daftar$no_dokumen)
  out <- list()
  tambah <- function(dok, judul, tingkat, pesan) out[[length(out) + 1]] <<- data.frame(no_dokumen = dok, judul = judul, tingkat = tingkat, pesan = pesan, stringsAsFactors = FALSE)

  ganda <- unique(daftar$no_dokumen[duplicated(daftar$no_dokumen) & !is.na(daftar$no_dokumen)])
  for (g in ganda) tambah(g, paste(daftar$judul[daftar$no_dokumen %in% g], collapse = " / "), "merah", "Nomor dokumen dipakai lebih dari satu SOP")

  for (i in seq_len(nrow(daftar))) {
    d <- daftar[i, ]; st <- tolower(d$status %||% "")
    if (grepl("belum", st)) tambah(d$no_dokumen, d$judul, "merah", "Status: Belum Upload")
    else if (grepl("revisi|review", st)) tambah(d$no_dokumen, d$judul, "kuning", paste("Status:", d$status))
    kf <- .kode_dok(d$nama_file %||% "")
    if (!is.na(kf) && !is.na(d$kode) && kf != d$kode) {
      tambah(d$no_dokumen, d$judul, "merah", sprintf("Nama file (%s) memakai nomor %s, tidak sama dengan nomor dokumen %s", d$nama_file, kf, d$kode))
    }
    norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    if (!is.na(d$nama_file) && nzchar(d$nama_file) && !grepl(substr(norm(sub("^Prosedur Mutu ", "", d$judul)), 1, 15), norm(d$nama_file), fixed = TRUE)) {
      tambah(d$no_dokumen, d$judul, "kuning", sprintf("Nama file '%s' tampaknya tidak sesuai judul", d$nama_file))
    }
    if (nrow(pdf)) {
      p <- pdf[!is.na(pdf$kode) & pdf$kode == d$kode, ]
      if (nrow(p) == 0) {
        if (!grepl("belum", st)) tambah(d$no_dokumen, d$judul, "merah", "PDF dengan nomor dokumen ini tidak ditemukan di antara file yang diperiksa")
      } else {
        v <- suppressWarnings(as.numeric(d$versi))
        for (j in seq_len(nrow(p))) {
          if (!is.na(v) && !is.na(p$revisi[j]) && as.integer(v) != p$revisi[j]) {
            tambah(d$no_dokumen, d$judul, "merah", sprintf("Versi di daftar %s, tetapi nomor revisi di PDF %02d (%s)", d$versi, p$revisi[j], p$nama[j]))
          }
          if (!is.na(v) && !is.na(p$versi_nama_file[j]) && v != p$versi_nama_file[j]) {
            tambah(d$no_dokumen, d$judul, "kuning", sprintf("Versi di nama file v%s berbeda dengan daftar (%s)", p$versi_nama_file[j], d$versi))
          }
          if (!is.na(p$kode_nama_file[j]) && p$kode_nama_file[j] != p$kode[j]) {
            tambah(d$no_dokumen, d$judul, "merah", sprintf("File '%s': nomor di nama file (%s) berbeda dengan nomor di isi PDF (%s)", p$nama[j], p$kode_nama_file[j], p$kode[j]))
          }
          if (nzchar(p$masalah[j])) tambah(d$no_dokumen, d$judul, "kuning", paste0(p$nama[j], ": ", p$masalah[j]))
        }
      }
    }
  }
  if (nrow(pdf)) {
    yatim <- pdf[is.na(pdf$kode) | !(pdf$kode %in% daftar$kode), ]
    for (j in seq_len(nrow(yatim))) tambah(yatim$no_dok[j] %||% "-", yatim$nama[j], "kuning", "PDF tidak tercantum di daftar PM")
  }
  temuan <- if (length(out)) unique(do.call(rbind, out)) else data.frame(no_dokumen = character(0), judul = character(0), tingkat = character(0), pesan = character(0))
  list(temuan = temuan, pdf = pdf)
}
