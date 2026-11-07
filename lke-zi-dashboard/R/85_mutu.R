# =============================================================================
# 85_mutu.R - Penjaminan mutu: siklus PDCA, kendali dokumen, keselarasan ISO,
#             analisis sensitivitas skor, dan evaluasi kinerja validator
# -----------------------------------------------------------------------------
# Landasan rancangan (hasil telaah repositori bukti unit dan riwayat penilaian):
#  1. Sebagian besar catatan penurunan nilai oleh TPI menyangkut unsur
#     pemantauan-evaluasi (C) dan tindak lanjut (A) yang tidak terbukti.
#  2. Kelengkapan tahap PDCA yang diturunkan dari NAMA dokumen TIDAK berkorelasi
#     positif dengan nilai TPI (butir dengan 3-4 tahap justru seluruhnya
#     diturunkan), kemungkinan karena kausalitas terbalik: dokumen revisi
#     ditambahkan setelah butir diturunkan. Karena itu indikator PDCA di sini
#     diperlakukan sebagai DAFTAR PERIKSA kelengkapan, bukan prediktor nilai.
#  3. Hampir separuh nama dokumen tidak memuat tahun dan sebagian bersufiks
#     unduhan berulang, misalnya "(1)", sehingga identifikasi dokumen (ISO
#     9001:2015 klausul 7.5.2) belum terkendali.
# =============================================================================

# ---------------------------------------------------------------- Siklus PDCA
TAHAP_PDCA <- list(
  P = list(label = "Perencanaan/penetapan", contoh = "SK, pedoman, kebijakan, rencana kerja, standar",
           pola = "(?<![a-z])sk(?![a-z])|surat keputusan|pedoman|kebijakan|peraturan|standar|rencana|renstra|perjanjian kinerja|maklumat|penetapan|road ?map|panduan|(?<![a-z])sop(?![a-z])|prosedur"),
  D = list(label = "Pelaksanaan", contoh = "undangan, daftar hadir, notulen, dokumentasi, laporan kegiatan",
           pola = "undangan|daftar hadir|presensi|absensi|notulen|notula|dokumentasi|foto|laporan kegiatan|sosialisasi|pelaksanaan|workshop|rapat|pelatihan|bimtek|implementasi"),
  C = list(label = "Pemantauan dan evaluasi", contoh = "laporan monev, reviu, survei, audit, analisis capaian",
           pola = "monev|monitoring|evaluasi|reviu|review|survei|survey|audit|pengukuran|capaian|analisis"),
  A = list(label = "Tindak lanjut", contoh = "laporan tindak lanjut, rencana aksi perbaikan, dokumen revisi",
           pola = "tindak ?lanjut|(?<![a-z])tl(?![a-z])|rencana aksi|perbaikan|revisi|(?<![a-z])rev[._ ]")
)

# Tahap PDCA yang tercermin pada daftar judul bukti (dan narasi, bila ada)
tahap_pdca <- function(judul, narasi = "") {
  teks <- tolower(paste(c(judul, narasi), collapse = " \n "))
  stats::setNames(vapply(TAHAP_PDCA, function(t) grepl(t$pola, teks, perl = TRUE), logical(1)), names(TAHAP_PDCA))
}

# ------------------------------------------------- Kendali identifikasi dokumen
# Konvensi nama yang dianjurkan: TAHUN_KODE_JENIS_URAIAN
#   contoh: 2026_P1.i.a_SK_Pembentukan Tim Pembangunan ZI
POLA_NAMA_TIDAK_TERKENDALI <- "\\(\\d+\\)|(?<![a-z])copy(?![a-z])|salinan|^scan|^img[_-]|untitled|new update|final ?final"

cek_kendali_nama <- function(judul, kode) {
  hasil <- list()
  tidak_terkendali <- judul[grepl(POLA_NAMA_TIDAK_TERKENDALI, tolower(judul), perl = TRUE)]
  if (length(tidak_terkendali)) hasil$sufiks <- tidak_terkendali
  kode_rx <- gsub(".", "\\.", kode, fixed = TRUE)
  tanpa_kode <- judul[!grepl(kode_rx, judul, ignore.case = TRUE)]
  if (length(tanpa_kode) == length(judul) && length(judul)) hasil$tanpa_kode <- TRUE
  hasil
}

# ------------------------------------------------------- Keselarasan ISO
# Nama klausul per standar (nomor yang sama dapat bermakna berbeda antarstandar)
KLAUSUL_ISO <- list(
  iso9001 = c(
    "4.4" = "Sistem manajemen mutu dan prosesnya", "5.1" = "Kepemimpinan dan komitmen",
    "5.3" = "Peran, tanggung jawab, dan wewenang organisasi", "6.1" = "Tindakan untuk menangani risiko dan peluang",
    "6.2" = "Sasaran mutu dan perencanaan pencapaiannya", "7.1.2" = "Sumber daya manusia", "7.1.3" = "Infrastruktur",
    "7.2" = "Kompetensi", "7.3" = "Kepedulian", "7.4" = "Komunikasi", "7.5" = "Informasi terdokumentasi",
    "8.1" = "Perencanaan dan pengendalian operasional", "8.2" = "Persyaratan produk dan layanan",
    "8.5" = "Produksi dan penyediaan layanan", "9.1" = "Pemantauan, pengukuran, analisis, dan evaluasi",
    "9.1.2" = "Kepuasan pelanggan", "9.1.3" = "Analisis dan evaluasi", "9.2" = "Audit internal",
    "9.3" = "Tinjauan manajemen", "10.2" = "Ketidaksesuaian dan tindakan korektif", "10.3" = "Peningkatan berkelanjutan"),
  iso37001 = c(
    "4.5" = "Penilaian risiko penyuapan", "5.1" = "Kepemimpinan dan komitmen", "5.2" = "Kebijakan anti-penyuapan",
    "5.3.2" = "Fungsi kepatuhan anti-penyuapan", "6.2" = "Sasaran anti-penyuapan dan perencanaan pencapaiannya",
    "7.1" = "Sumber daya", "7.2" = "Kompetensi", "7.2.2" = "Proses kepegawaian", "7.3" = "Kepedulian dan pelatihan",
    "7.4" = "Komunikasi", "7.5" = "Informasi terdokumentasi", "8.1" = "Perencanaan dan pengendalian operasional",
    "8.3" = "Pengendalian finansial", "8.4" = "Pengendalian non-finansial",
    "8.7" = "Hadiah, jamuan, donasi, dan manfaat serupa", "8.9" = "Pelaporan dugaan penyuapan",
    "8.10" = "Investigasi dan penanganan penyuapan", "9.1" = "Pemantauan, pengukuran, analisis, dan evaluasi",
    "9.2" = "Audit internal", "9.3" = "Tinjauan manajemen", "10.1" = "Ketidaksesuaian dan tindakan korektif",
    "10.2" = "Peningkatan berkelanjutan"))

muat_pemetaan_iso <- function(path = "data/pemetaan_iso.csv") {
  m <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", colClasses = "character")
  m[is.na(m)] <- ""
  m
}

# Kesiapan per klausul: proporsi butir terkait yang berstatus hijau
kesiapan_iso <- function(status, rubrik, pemetaan) {
  st <- merge(status[, c("kode", "status")], unique(rubrik[rubrik$tipe == "butir", c("kode", "sub_kode")]), by = "kode")
  baris <- list()
  for (std in c("iso9001", "iso37001")) {
    for (i in seq_len(nrow(pemetaan))) {
      kl <- trimws(unlist(strsplit(pemetaan[[std]][i], ";")))
      kl <- kl[nzchar(kl)]
      if (!length(kl)) next
      s <- st$status[st$sub_kode == pemetaan$sub_kode[i]]
      for (k in kl) baris[[length(baris) + 1]] <- data.frame(standar = std, klausul = k, sub_kode = pemetaan$sub_kode[i],
                                                           status = s, stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, baris)
  a <- stats::aggregate(cbind(butir = 1, hijau = d$status == "hijau", merah = d$status == "merah",
                              belum = d$status == "belum diisi") ~ standar + klausul, data = d, FUN = sum)
  a$kesiapan <- round(100 * a$hijau / a$butir)
  a$nama <- mapply(function(std, k) KLAUSUL_ISO[[std]][k] %||% "", a$standar, a$klausul, USE.NAMES = FALSE)
  a$nama[is.na(a$nama)] <- ""
  urut <- function(x) vapply(strsplit(x, ".", fixed = TRUE), function(p) sum(as.numeric(p) * 100^-(seq_along(p) - 1)), numeric(1))
  a <- a[order(a$standar, urut(a$klausul)), ]
  a$standar <- ifelse(a$standar == "iso9001", "ISO 9001:2015", "ISO 37001:2016")
  rownames(a) <- NULL
  a
}

# -------------------------------------------------- Analisis sensitivitas skor
# Tiga skenario (BUKAN prediksi):
#  - klaim       : nilai sesuai jawaban unit
#  - konservatif : butir merah dinilai 0, butir kuning turun satu tingkat
#  - historis    : nilai klaim per komponen dikali rasio (nilai TPI / klaim unit)
#                  pada penilaian terdahulu
turun_satu <- function(pilihan, jawaban) {
  h <- pilihan_jawaban(pilihan); i <- match(jawaban, h)
  if (is.na(i)) return(jawaban)
  h[min(length(h), i + 1)]
}

skenario_skor <- function(rubrik, isian, status, riwayat = NULL) {
  j <- jawaban_dari_isian(isian)
  klaim <- hitung_skor(rubrik, j)
  jk <- j
  for (k in status$kode[status$status %in% c("merah", "kuning")]) {
    b <- rubrik[rubrik$kode == k, ]
    if (!(b$pilihan %in% names(PETA_NILAI)) || !(k %in% names(jk))) next
    jk[[k]] <- if (status$status[status$kode == k] == "merah") utils::tail(pilihan_jawaban(b$pilihan), 1) else turun_satu(b$pilihan, jk[[k]])
  }
  kons <- hitung_skor(rubrik, jk)
  hasil <- data.frame(skenario = c("Klaim unit", "Konservatif"),
                      pengungkit = c(klaim$ringkas$skor[3], kons$ringkas$skor[3]),
                      hasil = c(klaim$ringkas$skor[4], kons$ringkas$skor[4]))
  if (!is.null(riwayat) && any(nzchar(riwayat$jawaban_tpi))) {
    u <- hitung_skor(rubrik, stats::setNames(riwayat$jawaban_unit, riwayat$kode), FALSE)$komponen
    t <- hitung_skor(rubrik, stats::setNames(riwayat$jawaban_tpi, riwayat$kode), FALSE)$komponen
    rasio <- ifelse(u$skor > 0, pmin(1, t$skor / u$skor), 1)
    ks <- klaim$komponen; r <- rasio[match(paste(ks$area_kode, ks$komponen_no), paste(u$area_kode, u$komponen_no))]
    hasil <- rbind(hasil, data.frame(skenario = "Rasio historis TPI", pengungkit = sum(ks$skor * ifelse(is.na(r), 1, r)),
                                     hasil = klaim$ringkas$skor[4]))
  }
  hasil
}

# --------------------------------------------- Evaluasi kinerja validator
# Membandingkan bendera validator (merah/kuning) dengan penurunan oleh TPI
# pada data riwayat. Menghasilkan matriks konfusi dan ukuran diagnostik.
evaluasi_validator <- function(rubrik, isian_riwayat, riwayat, tahun) {
  tm <- validasi_semua(rubrik, isian_riwayat, NULL, tahun)
  bt <- rubrik[rubrik$tipe == "butir" & rubrik$pilihan %in% names(PETA_NILAI), c("kode", "pilihan")]
  x <- merge(bt, riwayat[, c("kode", "jawaban_unit", "jawaban_tpi")], by = "kode")
  x$nu <- mapply(nilai_pilihan, x$pilihan, x$jawaban_unit); x$nt <- mapply(nilai_pilihan, x$pilihan, x$jawaban_tpi)
  x <- x[!is.na(x$nu) & !is.na(x$nt), ]
  if (nrow(x) == 0) return(NULL)
  x$turun <- x$nt < x$nu
  x$bendera <- x$kode %in% tm$kode[tm$tingkat %in% c("merah", "kuning")]
  tp <- sum(x$turun & x$bendera); fp <- sum(!x$turun & x$bendera); fn <- sum(x$turun & !x$bendera); tn <- sum(!x$turun & !x$bendera)
  list(n = nrow(x), tp = tp, fp = fp, fn = fn, tn = tn,
       prevalensi = mean(x$turun),
       sensitivitas = if (tp + fn) tp / (tp + fn) else NA,
       spesifisitas = if (tn + fp) tn / (tn + fp) else NA,
       ppv = if (tp + fp) tp / (tp + fp) else NA)
}

# Interval kepercayaan Wilson 95% untuk proporsi
ci_wilson <- function(x, n, z = 1.96) {
  if (n == 0) return(c(NA, NA))
  p <- x / n; d <- 1 + z^2 / n
  pusat <- (p + z^2 / (2 * n)) / d; lebar <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(max(0, pusat - lebar), min(1, pusat + lebar))
}
