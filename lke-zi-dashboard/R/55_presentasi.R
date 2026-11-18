# =============================================================================
# 55_presentasi.R - Presentasi PowerPoint hasil pra-penilaian (untuk paparan
#                   pimpinan) berbasis templat FK Unhas
# -----------------------------------------------------------------------------
# Templat dicari di KONFIG$templat_pptx (bawaan www/kustom/Template_FK_Unhas.pptx,
# tidak ikut ke GitHub). Lima slide pertama templat dibaca sebagai desain:
#   1 sampul (panel merah kiri)      2 kepala merah (simpulan)
#   3 pembatas bagian (diagonal)     4 isi polos ber-footer (latar master)
#   5 penutup (merah penuh)
# Gambar latar setiap desain dipasang ulang pada slide baru, lalu lima slide
# contoh templat dihapus. Bila templat tidak ditemukan, presentasi tetap dibuat
# dengan templat bawaan officer dan warna FK.
# Grafik berupa gambar 300 dpi (paket mschart/rvg tidak diwajibkan); teks dan
# tabel tetap dapat disunting di PowerPoint.
# =============================================================================

MERAH_FK <- "#B5090A"
.PPT_GELAP <- "#2B2B2B"
.PPT_REDUP <- "#6B6B6B"
.PPT_MUDA  <- "#F8E6E6"

.angka_id <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

.tanggal_id <- function(t = Sys.Date()) {
  bulan <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus",
             "September", "Oktober", "November", "Desember")
  paste(as.integer(format(t, "%d")), bulan[as.integer(format(t, "%m"))], format(t, "%Y"))
}

# Gambar latar setiap desain dalam templat (berkas PNG sementara)
.latar_templat <- function(templat) {
  tmp <- tempfile("templat_"); dir.create(tmp)
  utils::unzip(templat, exdir = tmp)
  latar_slide <- function(i) {
    rels <- file.path(tmp, "ppt", "slides", "_rels", sprintf("slide%d.xml.rels", i))
    if (!file.exists(rels)) return(NA_character_)
    xml <- paste(readLines(rels, warn = FALSE), collapse = "")
    t <- regmatches(xml, regexpr('Target="\\.\\./media/[^"]+"', xml))
    if (!length(t)) return(NA_character_)
    f <- file.path(tmp, "ppt", "media", basename(gsub('^Target="|"$', "", t)))
    if (file.exists(f)) f else NA_character_
  }
  list(sampul = latar_slide(1), kepala = latar_slide(2), pembatas = latar_slide(3),
       penutup = latar_slide(5))
}

.fp <- function(ukuran = 14, warna = .PPT_GELAP, tebal = FALSE, miring = FALSE)
  officer::fp_text(font.size = ukuran, color = warna, bold = tebal, italic = miring, font.family = "Calibri")

.par_ppt <- function(teks, ukuran = 14, warna = .PPT_GELAP, tebal = FALSE, rata = "left", miring = FALSE, jarak = 4)
  officer::fpar(officer::ftext(teks, .fp(ukuran, warna, tebal, miring)),
                fp_p = officer::fp_par(text.align = rata, padding.bottom = jarak))

.kotak <- function(pr, isi, l, t, w, h, bg = NULL) {
  if (inherits(isi, "fpar")) isi <- officer::block_list(isi)
  officer::ph_with(pr, isi, location = officer::ph_location(left = l, top = t, width = w, height = h, bg = bg))
}

.gambar <- function(pr, berkas, l, t, w, h)
  officer::ph_with(pr, officer::external_img(berkas, width = w, height = h),
                   location = officer::ph_location(left = l, top = t, width = w, height = h), use_loc_size = TRUE)

.judul_isi <- function(pr, teks) .kotak(pr, .par_ppt(teks, 22, MERAH_FK, TRUE), 0.5, 0.3, 9, 0.8)

.slide_baru <- function(pr, latar = NA_character_, info) {
  pr <- officer::add_slide(pr, layout = info$layout, master = info$master)
  if (!is.na(latar)) pr <- .gambar(pr, latar, 0, 0, info$lebar, info$tinggi)
  pr
}

.tabel_ppt <- function(pr, df, l, t, lebar_kolom, ukuran = 12) {
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::font(ft, fontname = "Calibri", part = "all")
    ft <- flextable::fontsize(ft, size = ukuran, part = "all")
    ft <- flextable::bg(ft, bg = MERAH_FK, part = "header")
    ft <- flextable::color(ft, color = "#FFFFFF", part = "header")
    ft <- flextable::bold(ft, part = "header")
    if (nrow(df) > 1) ft <- flextable::bg(ft, i = seq(2, nrow(df), by = 2), bg = .PPT_MUDA, part = "body")
    ft <- flextable::color(ft, color = .PPT_GELAP, part = "body")
    ft <- flextable::border_remove(ft)
    ft <- flextable::hline(ft, border = officer::fp_border(color = "#E3C9C9", width = 0.75), part = "body")
    ft <- flextable::valign(ft, valign = "center", part = "all")
    ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3, padding.left = 6, padding.right = 6, part = "all")
    ft <- flextable::width(ft, width = lebar_kolom)
    officer::ph_with(pr, ft, location = officer::ph_location(left = l, top = t, width = sum(lebar_kolom), height = 0.4))
  } else {
    officer::ph_with(pr, df, location = officer::ph_location(left = l, top = t, width = sum(lebar_kolom), height = 0.4))
  }
}

.penanda <- function(pr, teks, l, t, w, ukuran = 15) {
  pr <- .kotak(pr, .par_ppt(" ", 6), l, t + 0.09, 0.13, 0.13, bg = MERAH_FK)
  .kotak(pr, .par_ppt(teks, ukuran, .PPT_GELAP), l + 0.28, t - 0.04, w - 0.28, 0.6)
}

.png_tmp <- function() tempfile(fileext = ".png")

.grafik_komponen_png <- function(rubrik, skor, riwayat, berkas, w, h) {
  kp <- skor$komponen
  lab <- paste0("Zona ", kp$komponen_no, " (", kp$area_kode, ")")
  d <- data.frame(komponen = lab, sumber = "Klaim saat ini", persen = 100 * kp$skor / kp$bobot_komponen)
  if (!is.null(riwayat) && "jawaban_tpi" %in% names(riwayat) && any(nzchar(riwayat$jawaban_tpi))) {
    k2 <- hitung_skor(rubrik, stats::setNames(riwayat$jawaban_tpi, riwayat$kode), kosong_nol = FALSE)$komponen
    d <- rbind(d, data.frame(komponen = paste0("Zona ", k2$komponen_no, " (", k2$area_kode, ")"), sumber = "TPI (periode lalu)",
                             persen = 100 * k2$skor / k2$bobot_komponen))
  }
  d$sumber <- factor(d$sumber, c("TPI (periode lalu)", "Klaim saat ini"))
  d$komponen <- factor(d$komponen, rev(sort(unique(d$komponen))))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = persen, y = komponen, fill = sumber)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(formatC(persen, format = "f", digits = 0), "%")),
                       position = ggplot2::position_dodge(width = 0.78), hjust = -0.15, size = 3.2, colour = "#4A4A4A") +
    ggplot2::scale_fill_manual(values = c("TPI (periode lalu)" = "#BDBDBD", "Klaim saat ini" = MERAH_FK), drop = TRUE) +
    ggplot2::scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Persentase terhadap bobot komponen", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.text = ggplot2::element_text(colour = "#3A3A3A"))
  ggplot2::ggsave(berkas, g, width = w, height = h, dpi = 300, bg = "white")
  berkas
}

.grafik_status_png <- function(status, butir, berkas, w, h) {
  st <- merge(status, unique(butir[, c("kode", "zona", "nama_zona")]), by = "kode")
  st$komponen_lab <- ifelse(st$zona == "H", "Hasil", paste0(st$nama_zona, " (", substr(st$area, 1, 1), ")"))
  tingkat <- c("hijau", "kuning", "merah", "belum diisi")
  st$status <- factor(st$status, rev(tingkat))
  st$komponen_lab <- factor(st$komponen_lab, rev(sort(unique(st$komponen_lab))))
  g <- ggplot2::ggplot(st, ggplot2::aes(y = komponen_lab, fill = status)) +
    ggplot2::geom_bar(width = 0.7) +
    ggplot2::scale_fill_manual(values = c(hijau = "#1E7B4A", kuning = "#D99A00", merah = "#B42318", `belum diisi` = "#C9C9C9"),
                               breaks = tingkat, labels = c("Hijau", "Kuning", "Merah", "Belum diisi"), drop = FALSE) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = "Jumlah butir", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.text = ggplot2::element_text(colour = "#3A3A3A"))
  ggplot2::ggsave(berkas, g, width = w, height = h, dpi = 300, bg = "white")
  berkas
}

laporan_presentasi_pptx <- function(rubrik, isian, status, temuan, skor, keluaran, tahun = KONFIG$tahun,
                                    capa = NULL, skenario = NULL, riwayat = NULL, butir = NULL,
                                    templat = KONFIG$templat_pptx, penyaji = "[Nama dan jabatan penyaji]") {
  ada_templat <- !is.null(templat) && nzchar(templat) && file.exists(templat)
  pr <- if (ada_templat) officer::read_pptx(templat) else officer::read_pptx()
  n_awal <- length(pr)
  uk <- officer::slide_size(pr)
  ls <- officer::layout_summary(pr)
  layout_kosong <- if ("Blank" %in% ls$layout) "Blank" else ls$layout[1]
  info <- list(layout = layout_kosong, master = ls$master[ls$layout == layout_kosong][1], lebar = uk$width, tinggi = uk$height)
  latar <- if (ada_templat) .latar_templat(templat) else list(sampul = NA, kepala = NA, pembatas = NA, penutup = NA)
  W <- uk$width; H <- uk$height
  if (is.null(butir)) { butir <- rubrik[rubrik$tipe == "butir", ]
    butir$zona <- ifelse(butir$bagian == "Hasil", "H", paste0("Z", butir$komponen_no))
    butir$nama_zona <- ifelse(butir$bagian == "Hasil", "Hasil", paste("Zona", butir$komponen_no)) }
  if (is.null(skenario)) skenario <- skenario_skor(rubrik, isian, status, riwayat)

  # ----------------------------------------------------------- angka utama
  n <- nrow(status)
  tab <- table(factor(status$status, c("hijau", "kuning", "merah", "belum diisi")))
  sk_p <- skor$ringkas$skor[3]; sk_h <- skor$ringkas$skor[4]; total <- sk_p + sk_h
  tot_sken <- skenario$pengungkit + skenario$hasil
  tm <- temuan[temuan$tingkat %in% c("merah", "kuning"), ]
  per_aturan <- if (nrow(tm)) sort(tapply(tm$kode, tm$aturan, function(k) length(unique(k))), decreasing = TRUE) else integer(0)
  kp <- skor$komponen; kp$persen <- 100 * kp$skor / kp$bobot_komponen
  kp$label <- paste0("Zona ", kp$komponen_no, " (", kp$area_kode, ") ", kp$komponen)
  rendah <- kp[order(kp$persen), ][1, ]
  n_capa <- if (!is.null(capa)) nrow(capa) else 0
  n_buka <- if (n_capa) sum(capa$status %in% c("Terbuka", "Dalam proses")) else 0
  catatan <- list()

  # ------------------------------------------------------------- 1. sampul
  pr <- .slide_baru(pr, latar$sampul, info)
  x0 <- if (ada_templat) 4.0 else 0.6; lebar_teks <- W - x0 - 0.5
  pr <- .kotak(pr, .par_ppt("LAPORAN PRA-PENILAIAN MANDIRI", 13, MERAH_FK, TRUE), x0, 1.15, lebar_teks, 0.45)
  pr <- .kotak(pr, officer::block_list(
    .par_ppt("Lembar Kerja Evaluasi", 30, .PPT_GELAP, TRUE, jarak = 0),
    .par_ppt(sprintf("Zona Integritas %d", tahun), 30, .PPT_GELAP, TRUE, jarak = 0)), x0, 1.6, lebar_teks, 1.3)
  pr <- .kotak(pr, .par_ppt(KONFIG$unit, 16, .PPT_REDUP), x0, 3.0, lebar_teks, 0.5)
  pr <- .kotak(pr, officer::block_list(.par_ppt(penyaji, 14, .PPT_GELAP, TRUE), .par_ppt(.tanggal_id(), 13, .PPT_REDUP)),
               x0, 4.3, lebar_teks, 0.8)
  catatan[[length(catatan) + 1]] <- sprintf(paste(
    "Pembukaan. Laporan ini merangkum pra-penilaian mandiri LKE Zona Integritas %d sebelum dinilai Tim Penilai.",
    "Angka yang disajikan adalah skor klaim unit, bukan prediksi nilai resmi."), tahun)

  # ------------------------------------------------- 2. ringkasan eksekutif
  pr <- .slide_baru(pr, NA_character_, info)
  pr <- .judul_isi(pr, sprintf("Skor klaim %s dari 100; %d dari %d butir lolos validasi", .angka_id(total), tab[["hijau"]], n))
  kartu <- list(c(.angka_id(sk_p), "Pengungkit (dari 60)"), c(.angka_id(sk_h), "Hasil (dari 40)"),
                c(sprintf("%d/%d", tab[["hijau"]], n), "Butir hijau"), c(sprintf("%d", tab[["merah"]] + tab[["kuning"]]), "Butir merah + kuning"))
  lk <- (9 - 3 * 0.25) / 4
  for (i in seq_along(kartu)) {
    pr <- .kotak(pr, officer::block_list(.par_ppt(kartu[[i]][1], 30, "#FFFFFF", TRUE, "center", jarak = 0),
                                        .par_ppt(kartu[[i]][2], 13, "#FFFFFF", FALSE, "center", jarak = 0)),
                 0.5 + (i - 1) * (lk + 0.25), 1.35, lk, 1.25, bg = if (i == 4) "#7A1F1F" else MERAH_FK)
  }
  poin <- c(
    sprintf("Skor dibaca sebagai rentang %s\u2013%s, bergantung pada butir berisiko yang diturunkan evaluator.",
            .angka_id(min(tot_sken)), .angka_id(max(tot_sken))),
    sprintf("%d butir merah wajib diselesaikan sebelum unggah; %d butir kuning berisiko diturunkan.", tab[["merah"]], tab[["kuning"]]),
    sprintf("Komponen dengan capaian klaim terendah: %s (%s%%).", rendah$label, .angka_id(rendah$persen, 0)),
    sprintf("%d tindakan korektif tercatat; %d masih terbuka atau dalam proses.", n_capa, n_buka))
  for (i in seq_along(poin)) pr <- .penanda(pr, poin[i], 0.5, 2.95 + (i - 1) * 0.6, 9)
  catatan[[length(catatan) + 1]] <- sprintf(paste(
    "Ringkasan. Skor klaim Pengungkit %s dari 60 dan Hasil %s dari 40. %d dari %d butir lolos seluruh aturan validasi.",
    "Karena sebagian butir masih merah atau kuning, skor sebaiknya disampaikan sebagai rentang."), .angka_id(sk_p), .angka_id(sk_h), tab[["hijau"]], n)

  # --------------------------------------------------------- 3. pembatas
  pembatas <- function(pr, nomor, judul, sub) {
    pr <- .slide_baru(pr, latar$pembatas, info)
    lebar <- if (ada_templat) 4.6 else 8.5
    pr <- .kotak(pr, .par_ppt(nomor, 40, MERAH_FK, TRUE), 0.7, 1.7, lebar, 0.9)
    pr <- .kotak(pr, .par_ppt(judul, 30, .PPT_GELAP, TRUE), 0.7, 2.55, lebar, 1.3)
    .kotak(pr, .par_ppt(sub, 15, .PPT_REDUP), 0.7, 3.85, lebar, 0.9)
  }
  pr <- pembatas(pr, "01", "Capaian skor dan kesiapan bukti", "Skor per komponen, status validasi, dan analisis sensitivitas")
  catatan[[length(catatan) + 1]] <- "Bagian pertama: capaian skor dan kesiapan bukti per komponen."

  # ------------------------------------------------ 4. skor per komponen
  pr <- .slide_baru(pr, NA_character_, info)
  pr <- .judul_isi(pr, sprintf("%s memiliki capaian klaim terendah (%s%%)", sub("^(Zona [0-9]+ \\([PR]\\)).*$", "\\1", rendah$label), .angka_id(rendah$persen, 0)))
  g1 <- .grafik_komponen_png(rubrik, skor, riwayat, .png_tmp(), 6.1, 4.15)
  pr <- .gambar(pr, g1, 0.5, 1.25, 6.1, 4.15)
  urut <- kp[order(kp$persen), ]
  pr <- .kotak(pr, officer::block_list(
    .par_ppt("Terendah", 13, MERAH_FK, TRUE),
    .par_ppt(sprintf("%s: %s%%", sub("^(Zona [0-9]+ \\([PR]\\)).*$", "\\1", urut$label[1]), .angka_id(urut$persen[1], 0)), 14),
    .par_ppt(sprintf("%s: %s%%", sub("^(Zona [0-9]+ \\([PR]\\)).*$", "\\1", urut$label[2]), .angka_id(urut$persen[2], 0)), 14, jarak = 10),
    .par_ppt("Tertinggi", 13, MERAH_FK, TRUE),
    .par_ppt(sprintf("%s: %s%%", sub("^(Zona [0-9]+ \\([PR]\\)).*$", "\\1", utils::tail(urut$label, 1)), .angka_id(utils::tail(urut$persen, 1), 0)), 14, jarak = 10),
    .par_ppt("P = Pemenuhan, R = Reform. Persentase dihitung terhadap bobot setiap komponen.", 11, .PPT_REDUP, miring = TRUE)),
    6.85, 1.45, 2.65, 3.6, bg = .PPT_MUDA)
  catatan[[length(catatan) + 1]] <- sprintf("Skor per komponen. Capaian terendah pada %s sebesar %s%% dari bobotnya. Komponen ini menjadi prioritas pendampingan.",
                                            rendah$label, .angka_id(rendah$persen, 0))

  # --------------------------------------------------- 5. status validasi
  pr <- .slide_baru(pr, NA_character_, info)
  pr <- .judul_isi(pr, sprintf("%d butir merah dan %d kuning harus selesai sebelum unggah", tab[["merah"]], tab[["kuning"]]))
  g2 <- .grafik_status_png(status, butir, .png_tmp(), 6.1, 4.15)
  pr <- .gambar(pr, g2, 0.5, 1.25, 6.1, 4.15)
  arti <- list(c("Merah", "Syarat wajib belum terpenuhi; wajib diperbaiki."), c("Kuning", "Berisiko diturunkan; perbaiki atau jelaskan."),
               c("Hijau", "Lolos pemeriksaan bentuk dan kelengkapan."))
  pr <- .kotak(pr, officer::block_list(
    .par_ppt(sprintf("%d hijau \u00b7 %d kuning \u00b7 %d merah \u00b7 %d belum diisi", tab[["hijau"]], tab[["kuning"]], tab[["merah"]], tab[["belum diisi"]]), 13, .PPT_GELAP, TRUE, jarak = 10),
    .par_ppt(paste(arti[[1]], collapse = ": "), 12, jarak = 6), .par_ppt(paste(arti[[2]], collapse = ": "), 12, jarak = 6),
    .par_ppt(paste(arti[[3]], collapse = ": "), 12, jarak = 10),
    .par_ppt("Hijau belum berarti nilai penuh; kebenaran isi dokumen tetap diperiksa Tim Penilai.", 11, .PPT_REDUP, miring = TRUE)),
    6.85, 1.45, 2.65, 3.6, bg = .PPT_MUDA)
  catatan[[length(catatan) + 1]] <- sprintf("Status validasi. %d hijau, %d kuning, %d merah, dan %d belum diisi dari %d butir.",
                                            tab[["hijau"]], tab[["kuning"]], tab[["merah"]], tab[["belum diisi"]], n)

  # ------------------------------------------------ 6. analisis sensitivitas
  pr <- .slide_baru(pr, NA_character_, info)
  pr <- .judul_isi(pr, sprintf("Skor perlu dibaca sebagai rentang %s\u2013%s", .angka_id(min(tot_sken)), .angka_id(max(tot_sken))))
  dsk <- data.frame(Skenario = skenario$skenario, Pengungkit = .angka_id(skenario$pengungkit), Hasil = .angka_id(skenario$hasil),
                    Total = .angka_id(tot_sken), check.names = FALSE)
  pr <- .tabel_ppt(pr, dsk, 0.5, 1.45, c(3.2, 1.9, 1.9, 2.0), 14)
  ket <- c("Klaim unit: jawaban sebagaimana diisi.",
           "Konservatif: butir merah dinilai pada tingkat terendah dan butir kuning diturunkan satu tingkat.",
           if (nrow(skenario) > 2) "Rasio historis TPI: skor komponen dikoreksi dengan rasio nilai TPI terhadap klaim pada penilaian lalu.")
  y_ket <- 1.45 + (nrow(dsk) + 1) * 0.36 + 0.45
  for (i in seq_along(ket)) pr <- .penanda(pr, ket[i], 0.5, y_ket + (i - 1) * 0.55, 9, 13)
  catatan[[length(catatan) + 1]] <- "Analisis sensitivitas. Skenario konservatif menunjukkan batas bawah bila butir berisiko diturunkan evaluator."

  # --------------------------------------------------------- 7. pembatas
  pr <- pembatas(pr, "02", "Temuan dan tindak lanjut", "Pola temuan, butir prioritas, dan tindakan korektif")
  catatan[[length(catatan) + 1]] <- "Bagian kedua: temuan validasi dan rencana tindak lanjut."

  # ---------------------------------------------------- 8. temuan utama
  pr <- .slide_baru(pr, NA_character_, info)
  top <- utils::head(per_aturan, 5)
  porsi <- if (length(per_aturan)) round(100 * sum(utils::head(per_aturan, 3)) / sum(per_aturan)) else 0
  pr <- .judul_isi(pr, if (length(top)) sprintf("Tiga pola temuan teratas mencakup %d%% butir bermasalah", porsi) else "Tidak ada temuan merah atau kuning")
  if (length(top)) {
    dt <- data.frame(Kode = names(top), Temuan = unname(NAMA_ATURAN[names(top)]) %||% names(top),
                     `Jumlah butir` = as.integer(top),
                     `Tindak lanjut` = unname(ifelse(is.na(REKOMENDASI_ATURAN[names(top)]), "-", REKOMENDASI_ATURAN[names(top)])),
                     check.names = FALSE)
    dt$Temuan[is.na(dt$Temuan)] <- dt$Kode[is.na(dt$Temuan)]
    pr <- .tabel_ppt(pr, dt, 0.5, 1.35, c(0.8, 2.3, 1.2, 4.7), 12)
  }
  catatan[[length(catatan) + 1]] <- if (length(top)) sprintf("Temuan utama. Pola terbanyak: %s (%d butir).",
                                                              NAMA_ATURAN[names(top)[1]] %||% names(top)[1], top[[1]]) else "Tidak ada temuan merah atau kuning."

  # -------------------------------------------------- 9. butir prioritas
  merah <- status[status$status == "merah", ]
  if (nrow(merah)) {
    pr <- .slide_baru(pr, NA_character_, info)
    pr <- .judul_isi(pr, sprintf("%d butir merah menjadi prioritas perbaikan", nrow(merah)))
    tampil <- utils::head(merah$kode, 8)
    dm <- data.frame(Kode = tampil,
                     Butir = vapply(tampil, function(k) { q <- butir$pertanyaan[butir$kode == k][1]; if (nchar(q) > 70) paste0(substr(q, 1, 68), "\u2026") else q }, ""),
                     `Temuan utama` = vapply(tampil, function(k) { t <- tm[tm$kode == k & tm$tingkat == "merah", ]
                       if (nrow(t)) paste0(t$aturan[1], " ", NAMA_ATURAN[t$aturan[1]] %||% "") else "-" }, ""),
                     check.names = FALSE)
    pr <- .tabel_ppt(pr, dm, 0.5, 1.35, c(1.1, 5.0, 2.9), 12)
    if (nrow(merah) > 8) pr <- .kotak(pr, .par_ppt(sprintf("Ditambah %d butir merah lainnya; daftar lengkap pada laporan Word dan Excel.", nrow(merah) - 8), 12, .PPT_REDUP, miring = TRUE),
                                      0.5, 5.0, 9, 0.4)
    catatan[[length(catatan) + 1]] <- sprintf("Butir prioritas. %d butir merah memuat syarat wajib yang belum terpenuhi dan harus selesai sebelum unggah.", nrow(merah))
  }

  # ------------------------------------------------ 10. tindakan korektif
  pr <- .slide_baru(pr, NA_character_, info)
  pr <- .judul_isi(pr, if (n_capa) sprintf("%d tindakan korektif tercatat; %d masih terbuka", n_capa, n_buka) else "Tindakan korektif belum tercatat")
  if (n_capa) {
    st_capa <- table(factor(capa$status, STATUS_CAPA))
    lk2 <- (9 - 3 * 0.25) / 4
    for (i in seq_along(st_capa)) pr <- .kotak(pr, officer::block_list(
      .par_ppt(as.character(st_capa[[i]]), 26, "#FFFFFF", TRUE, "center", jarak = 0),
      .par_ppt(names(st_capa)[i], 12, "#FFFFFF", FALSE, "center", jarak = 0)),
      0.5 + (i - 1) * (lk2 + 0.25), 1.3, lk2, 0.95, bg = if (i <= 2) MERAH_FK else "#1E7B4A")
    cp <- capa[order(match(capa$status, STATUS_CAPA)), ]
    cp <- utils::head(cp, 5)
    dc <- data.frame(Kode = cp$kode, `Tindakan korektif` = ifelse(nchar(cp$tindakan) > 70, paste0(substr(cp$tindakan, 1, 68), "\u2026"), cp$tindakan),
                     PIC = cp$pic, Tenggat = ifelse(nzchar(cp$tenggat %||% ""), format(as.Date(cp$tenggat), "%d-%m-%Y"), "-"),
                     Status = cp$status, check.names = FALSE)
    pr <- .tabel_ppt(pr, dc, 0.5, 2.5, c(0.9, 4.0, 1.7, 1.1, 1.3), 11)
  } else {
    pr <- .penanda(pr, "Catat tindakan korektif untuk temuan yang bersifat sistemik melalui menu Penjaminan Mutu \u203a Tindakan Korektif.", 0.5, 1.6, 9)
  }
  catatan[[length(catatan) + 1]] <- sprintf("Tindakan korektif mengikuti klausul 10.2 ISO 9001:2015 dan 10.1 ISO 37001:2016. %d tercatat, %d masih terbuka.", n_capa, n_buka)

  # ------------------------------------- 11. simpulan dan rekomendasi
  pr <- .slide_baru(pr, latar$kepala, info)
  y0 <- if (ada_templat) 2.2 else 0.4
  pr <- .kotak(pr, .par_ppt("Simpulan dan rekomendasi", 24, MERAH_FK, TRUE), 0.6, y0, 8.8, 0.6)
  rek <- c(
    sprintf("Selesaikan %d butir merah sebelum batas unggah; tidak ada butir merah yang boleh diunggah.", tab[["merah"]]),
    if (length(per_aturan)) { k <- intersect(names(per_aturan), names(REKOMENDASI_ATURAN))[1]
      if (!is.na(k)) sprintf("Atasi pola temuan terbanyak (%s): %s%s", k, tolower(substr(REKOMENDASI_ATURAN[[k]], 1, 1)), substring(REKOMENDASI_ATURAN[[k]], 2)) },
    sprintf("Dampingi komponen dengan capaian terendah (%s).", sub("^(Zona [0-9]+ \\([PR]\\)).*$", "\\1", rendah$label)),
    "Laporkan skor kepada Tim Penilai sebagai rentang dan siapkan bukti atas setiap klaim tertinggi.")
  rek <- rek[!is.na(rek) & nzchar(rek)]
  rek <- utils::head(rek, 4)
  pr <- .kotak(pr, do.call(officer::block_list, lapply(seq_along(rek), function(i) officer::fpar(
    officer::ftext(sprintf("%d   ", i), .fp(15, MERAH_FK, TRUE)), officer::ftext(rek[i], .fp(14, .PPT_GELAP)),
    fp_p = officer::fp_par(padding.bottom = 10)))), 0.6, y0 + 0.65, 8.8, (if (ada_templat) 5.45 else 5.8) - (y0 + 0.65))
  catatan[[length(catatan) + 1]] <- "Simpulan dan rekomendasi untuk dibahas dan diputuskan."

  # -------------------------------------------------------- 12. penutup
  pr <- .slide_baru(pr, latar$penutup, info)
  pr <- .kotak(pr, .par_ppt("Terima kasih", 34, if (ada_templat) "#FFFFFF" else MERAH_FK, TRUE, "center"), 1.5, 0.9, 7, 0.9)
  catatan[[length(catatan) + 1]] <- "Penutup dan sesi tanya jawab."

  # ----------------------------------------- hapus slide contoh templat
  if (n_awal > 0) for (i in seq_len(n_awal)) pr <- officer::remove_slide(pr, index = 1)
  for (i in seq_along(catatan)) {
    pr <- officer::on_slide(pr, index = i)
    pr <- tryCatch(officer::set_notes(pr, value = catatan[[i]], location = officer::notes_location_type("body")),
                   error = function(e) pr)
  }
  print(pr, target = keluaran)
  .bersihkan_media_pptx(keluaran)
  invisible(keluaran)
}

# officer::remove_slide menghapus slide contoh templat tetapi meninggalkan gambar
# latarnya di ppt/media. Berkas media tanpa rujukan dibuang agar PowerPoint tidak
# menganggap presentasi rusak.
.bersihkan_media_pptx <- function(berkas) {
  tmp <- tempfile("pptx_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(berkas, exdir = tmp)
  media <- list.files(file.path(tmp, "ppt", "media"))
  if (!length(media)) return(invisible(berkas))
  rels <- list.files(tmp, pattern = "\\.rels$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  dirujuk <- unique(unlist(lapply(rels, function(r) {
    xml <- paste(readLines(r, warn = FALSE), collapse = "")
    basename(gsub('^Target="|"$', "", regmatches(xml, gregexpr('Target="[^"]*media/[^"]+"', xml))[[1]]))
  })))
  yatim <- setdiff(media, dirujuk)
  if (!length(yatim)) return(invisible(berkas))
  file.remove(file.path(tmp, "ppt", "media", yatim))
  tujuan <- normalizePath(berkas); file.remove(tujuan)
  zip::zipr(tujuan, files = list.files(tmp, all.files = TRUE, no.. = TRUE), root = tmp, include_directories = FALSE, mode = "mirror")
  invisible(berkas)
}
