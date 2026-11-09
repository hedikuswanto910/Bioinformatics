# Sistem Pendukung Keputusan Pengisian dan Pra-Penilaian Mandiri LKE Zona Integritas

Dokumentasi teknis aplikasi R Shiny untuk pengisian, validasi, dan penjaminan mutu Lembar Kerja Evaluasi (LKE) Zona Integritas menuju Wilayah Bebas dari Korupsi (WBK) dan Wilayah Birokrasi Bersih dan Melayani (WBBM).

## Ringkasan

Pada penilaian Zona Integritas, nilai unit kerja kerap diturunkan oleh Tim Penilai karena jawaban yang diklaim tidak didukung bukti yang dapat diverifikasi. Aplikasi ini dirancang untuk menutup kesenjangan tersebut melalui empat pendekatan:

1. **Pengisian terstruktur** per zona dan butir, dengan kriteria jawaban dan catatan evaluator terdahulu yang ditampilkan berdampingan.
2. **Validasi berbasis aturan** yang memeriksa bentuk dan kelengkapan isian secara seketika.
3. **Penjaminan mutu** berorientasi standar, mencakup daftar periksa siklus PDCA, pengendalian identifikasi dokumen, register tindakan korektif, dan pemetaan indikatif terhadap ISO 9001:2015 dan ISO 37001:2016.
4. **Pra-penilaian berbantuan model bahasa** (Claude) yang membedakan bukti terbukti, tidak terbukti, dan tidak dapat diverifikasi.

Aplikasi diposisikan sebagai alat bantu keputusan, bukan pengganti penilaian Tim Penilai.

## 1. Latar belakang dan rumusan masalah

Analisis atas riwayat penilaian unit menunjukkan pola yang konsisten: sebagian besar butir memperoleh nilai evaluator yang lebih rendah daripada klaim unit. Analisis isi catatan TPI dan TPSK mengelompokkan alasan penurunan ke dalam lima kategori:
- dokumen tidak tersedia atau tidak relevan;
- cakupan belum menyeluruh;
- inovasi tidak terbukti;
- tindak lanjut tidak dibuktikan;
- kegiatan tidak dilaksanakan secara berkala.

Dengan demikian, persoalan utamanya bukan kesalahan administratif pengunggahan, melainkan **ketidaksesuaian antara klaim dan bukti** (*overclaiming*).

## 2. Tujuan

1. Mengarahkan penanggung jawab butir (PIC) mengisi LKE secara berurutan dan konsisten dengan kriteria.
2. Mendeteksi ketidaksesuaian bentuk sejak dini, sebelum isian diajukan.
3. Menyediakan jejak audit, pengendalian dokumen, dan mekanisme tindakan korektif yang selaras dengan prinsip sistem manajemen mutu.
4. Menyajikan estimasi risiko secara jujur, termasuk ketidakpastian dan keterbatasannya.

## 3. Metode

### 3.1 Perhitungan skor

- Nilai butir kategorik dikonversi ke skala 0–1: Ya/Tidak = 1/0; A–C = 1/0,5/0; A–D = 1/0,67/0,33/0; A–E = 1/0,75/0,5/0,25/0.
- Butir persentase dihitung dari rinciannya.
- Skor sub-komponen adalah rerata nilai butir dikalikan bobot, dan skor komponen adalah jumlah skor sub-komponen.
- Mesin skor telah diverifikasi terhadap templat Excel resmi: skor Pengungkit untuk keempat penilai (unit, TP PTN, TPSK, TPI) identik dengan hasil rumus Excel.
- **Satu perbedaan disengaja:** butir kosong dihitung nol, sedangkan fungsi `AVERAGE()` di Excel mengabaikannya.

### 3.2 Validasi berbasis aturan

| Kode | Tingkat | Aturan |
|---|---|---|
| V01–V02 | merah | Jawaban, rincian, atau nilai survei belum diisi atau di luar rentang |
| V03 | merah | Penyebut persentase bernilai nol (menghasilkan `#DIV/0!` pada templat Excel) |
| V04 | kuning | Pembilang melampaui penyebut (persentase dibatasi 100%) |
| V05 | merah | Jumlah wajib lapor LHKPN/LHKASN tidak sama dengan agregat rinciannya |
| V06 | kuning | Data pelanggaran atau pengaduan tidak konsisten secara logis |
| V07 | merah | Rincian berlabel sama diisi berbeda antarbutir |
| V10 | merah/kuning | Narasi belum disusun atau kurang dari ambang minimal |
| V11–V12 | merah/kuning | Bukti tidak tersedia, tautan tidak valid, atau judul kurang deskriptif |
| V13 | kuning | Tidak terdapat bukti periode berjalan |
| V15 | kuning | Identifikasi dokumen belum terkendali, misalnya sufiks unduhan "(1)" atau salinan (ISO 9001:2015 klausul 7.5.2) |
| V16 | info | Nama dokumen belum memuat kode butir |
| V17 | info | Terdapat dokumen historis (tiga tahun atau lebih sebelum periode penilaian) |
| V20 | kuning | Unsur kriteria jawaban yang diklaim tidak tercermin pada narasi maupun judul bukti |
| V21 | info | Daftar periksa siklus PDCA: tahap yang belum tampak |
| V30 | info | Catatan evaluator terdahulu atas butir yang diturunkan |
| V31 | kuning | Klaim diajukan kembali dengan narasi yang identik dengan penilaian terdahulu |

**Status butir** ditentukan sebagai berikut:
- *merah* bila memuat sekurang-kurangnya satu temuan merah;
- *kuning* bila hanya memuat temuan kuning;
- *hijau* bila tidak memuat temuan merah maupun kuning.

Ambang angka diatur di `R/00_konfigurasi.R`.

### 3.3 Penjaminan mutu berorientasi standar

- **Daftar periksa siklus PDCA.** Judul bukti dan narasi diklasifikasikan ke tahap Perencanaan/penetapan, Pelaksanaan, Pemantauan-evaluasi, dan Tindak lanjut.
- **Pengendalian identifikasi dokumen** dengan konvensi nama `TAHUN_KODE_JENIS_URAIAN`, misalnya `2026_P1.i.a_SK_Pembentukan Tim Pembangunan ZI`.
- **Register tindakan korektif** (ISO 9001:2015 klausul 10.2; ISO 37001:2016 klausul 10.1).
  - Siklus status: Terbuka → Dalam proses → Selesai → Terverifikasi efektif.
  - Status *Selesai* mensyaratkan rumusan akar masalah.
  - Status *Terverifikasi efektif* mensyaratkan bukti penutupan.
- **Keselarasan ISO.** Setiap sub-komponen LKE dipetakan secara indikatif ke klausul ISO 9001:2015 dan ISO 37001:2016 (`data/pemetaan_iso.csv`). Kesiapan per klausul dihitung sebagai proporsi butir terkait yang berstatus hijau.
  - Pemetaan ini bukan pernyataan kesesuaian; kesesuaian hanya dapat ditetapkan melalui audit sertifikasi.

### 3.4 Analisis sensitivitas dan evaluasi validator

**Skenario skor.** Skor klaim disandingkan dengan dua skenario:
- *Skenario konservatif:* butir merah dinilai pada tingkat terendah, dan butir kuning diturunkan satu tingkat.
- *Skenario rasio historis:* skor klaim per komponen dikalikan rasio nilai TPI terhadap klaim pada penilaian terdahulu.

Kedua skenario merupakan ilustrasi rentang risiko, bukan prediksi.

**Evaluasi validator.** Kinerja diagnostik validator dievaluasi terhadap keputusan TPI pada data riwayat, menggunakan matriks konfusi, sensitivitas, spesifisitas, dan nilai prediktif positif. Setiap ukuran disertai interval kepercayaan Wilson 95%.

### 3.5 Pra-penilaian berbantuan Claude

- **Penilaian unsur kriteria.** Model menguraikan kriteria jawaban menjadi unsur yang dapat diuji, lalu menilai setiap unsur sebagai *terbukti*, *tidak terbukti*, atau *tidak dapat diverifikasi*.
- **Rujukan dan keyakinan.** Model menyebutkan dokumen yang menjadi dasar penilaian, menyatakan tingkat keyakinannya, dan menguji catatan evaluator terdahulu sebagai hipotesis kelemahan.
- **Larangan mengarang.** Draf narasi dan draf dokumen dilarang mengarang fakta; informasi yang belum tersedia ditandai `[PERLU DIISI]`.

## 4. Temuan pengembangan yang memengaruhi rancangan

1. **Kelengkapan nama dokumen bukan prediktor nilai.** Pada telaah repositori bukti unit, butir dengan tahap PDCA terlengkap pada nama dokumen justru seluruhnya diturunkan TPI.
   - Penjelasan yang paling mungkin adalah kausalitas terbalik: dokumen revisi ditambahkan setelah butir diturunkan.
   - Karena itu indikator PDCA hanya diposisikan sebagai daftar periksa, bukan skor.
2. **Validator memiliki kemampuan diskriminasi terbatas.** Nilai prediktif positifnya hanya sedikit melampaui prevalensi penurunan.
   - Validator efektif untuk mendeteksi ketidaksesuaian bentuk, tetapi penilaian substansi tetap memerlukan telaah isi dokumen.
3. **Identifikasi dokumen belum terkendali.** Hampir separuh nama dokumen bukti tidak memuat tahun, dan sebagian bersufiks unduhan berulang. Temuan ini mendasari aturan V15–V17.

## 5. Keterbatasan

- Validasi menilai bentuk dan kelengkapan, bukan substansi dokumen.
- Kategorisasi unsur kriteria dan tahap PDCA bergantung pada pencocokan kata kunci.
- Evaluasi validator bersifat dalam-sampel (*in-sample*), sehingga kinerja pada periode berikutnya berpotensi lebih rendah.
- Pemetaan ISO bersifat indikatif.
- Skor klaim dan skenario tidak dapat ditafsirkan sebagai prediksi nilai Tim Penilai.
- Aplikasi belum memiliki autentikasi pengguna. Pemakaian bersama memerlukan penempatan di server institusi di balik SSO atau VPN.

## 6. Instalasi dan penggunaan

**Prasyarat:** R 4.1 atau lebih baru; disarankan memakai RStudio.

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "readxl", "openxlsx", "httr2",
                   "jsonlite", "RSQLite", "DBI", "ggplot2", "pdftools", "officer", "testthat"))
```

**Menjalankan aplikasi:** buka `app.R` di RStudio lalu pilih **Run App**, atau jalankan:

```r
shiny::runApp("lke-zi-dashboard")
```

**Menyiapkan data** (sekali per periode; perintah dijalankan dari folder `lke-zi-dashboard`):

```bash
Rscript scripts/siapkan_data.R --riwayat "Indeks_Revisi_FK_UNHAS.xlsx"
```

```bash
Rscript scripts/siapkan_data.R --isian "Indeks_Revisi_FK_UNHAS.xlsx" --tahun 2027
```

Data internal disimpan di `data/lokal/` dan tidak disertakan ke GitHub.

**Pengaturan manual.** Menu **Pengaturan** disembunyikan secara bawaan agar parameter tidak dapat diubah dari peramban. Seluruh pengaturan dilakukan oleh Admin:

| Pengaturan | Cara mengubah |
|---|---|
| Tahun penilaian | `ZI_TAHUN` di `.Renviron`, lalu mulai ulang R |
| Tingkat penalaran Claude | `ZI_CLAUDE_EFFORT` di `.Renviron` (`low`, `medium`, `high`, `xhigh`) |
| Impor isian dan riwayat | `scripts/siapkan_data.R` (perintah di atas) |
| Logo dan banner | Format JPG, JPEG, atau PNG. Salin berkas ke `www/kustom/logo.png` dan `www/kustom/header.jpg` (nama harus `logo.*` dan `header.*`). Bila tidak ada, dipakai `www/gambar/logo_bawaan.png` dan `www/gambar/header_bawaan.jpg`. Teks nama unit di atas banner diatur dengan `ZI_TEKS_BANNER` (`otomatis`, `ya`, `tidak`) |
| Log biaya Claude dan jejak audit | Baca tabel `log_ai` dan `jejak` di `data/lokal/zi.sqlite`, misalnya `DBI::dbReadTable(DBI::dbConnect(RSQLite::SQLite(), "data/lokal/zi.sqlite"), "log_ai")` |

Untuk menampilkan kembali menu tersebut, misalnya hanya pada komputer Admin, tambahkan `ZI_MENU_PENGATURAN=ya` di `.Renviron`.

**Menghubungkan Claude (opsional):**
1. Salin `.Renviron.contoh` menjadi `.Renviron`.
2. Isi `ANTHROPIC_API_KEY` dengan kunci API dari console.anthropic.com. Kunci ini tidak sama dengan langganan Claude.ai.
3. Mulai ulang R.

Model bawaan adalah `claude-opus-5-5`, dengan *fallback* sisi server bila permintaan ditolak. Sebelum mengirim dokumen ke Claude, mintalah persetujuan pimpinan, dan hindari dokumen yang memuat data pribadi sensitif.

## 7. Navigasi aplikasi

| Menu | Isi |
|---|---|
| Beranda | Skor klaim, perbandingan dengan penilaian terdahulu, status per komponen, analisis sensitivitas |
| Pengisian LKE | Daftar butir per zona, kriteria, catatan evaluator, lencana klausul ISO, daftar periksa PDCA, validasi seketika, Claude |
| Penjaminan Mutu | Status validasi butir; register tindakan korektif; keselarasan ISO |
| Analisis | Analisis penilaian terdahulu; evaluasi kinerja validator |
| Dokumen | Draf dokumen berbasis fakta; pemeriksaan Prosedur Mutu; ekspor Excel dan Word |
| Glosarium Kode | Konvensi kode butir, rumusan lengkap, dan padanan klausul ISO |
| Pengaturan | Tersembunyi secara bawaan (`ZI_MENU_PENGATURAN=ya` untuk menampilkan): parameter, impor data, logo dan banner, pemakaian Claude, jejak audit |

**Ekspor Excel** berisi isian, temuan, rekap skor, dan register tindakan korektif.

**Laporan Word** berstruktur: pengendalian dokumen; pendahuluan; metode; hasil; pembahasan; keterbatasan; simpulan dan rekomendasi; serta lampiran temuan dan tindakan korektif.

**Tampilan** mengikuti identitas visual situs fakultas (ribbon merah, kartu marun, aksen emas) dan responsif:
- mobile: < 768 px;
- tablet: 768–1023 px;
- desktop: ≥ 1024 px, dengan lebar konten maksimum 1440 px.

## 8. Struktur berkas

```
app.R                   antarmuka dan alur server Shiny
R/00_konfigurasi.R      parameter (tahun, model, ambang validasi)
R/10_rubrik.R           rumus skor (identik templat Excel)
R/20_validasi.R         aturan validasi V01-V31
R/30_claude.R           integrasi Claude (keluaran JSON terstruktur)
R/40_penyimpanan.R      SQLite: isian, jejak audit, log AI, register tindakan korektif
R/50_ekspor.R           ekspor Excel dan laporan Word berstruktur akademis
R/60_sop.R              pemeriksaan daftar dan berkas Prosedur Mutu
R/70_impor.R            pembaca templat LKE Excel
R/80_tampilan.R         tema, header, dan ribbon
R/85_mutu.R             PDCA, kendali nama dokumen, keselarasan ISO, skenario skor, evaluasi validator
R/90_responsif.R        tata letak responsif
data/rubrik_lke.csv     113 butir dan 24 rincian beserta kriteria dan bobot
data/pemetaan_iso.csv   pemetaan indikatif sub-komponen ke klausul ISO 9001:2015 dan ISO 37001:2016
data/lokal/             data internal unit (tidak disertakan ke GitHub)
tests/testthat/         uji unit
tests/ui/               uji antarmuka responsif (Node.js dan Playwright)
```

**Pengujian:**

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## 9. Rujukan

- Peraturan Menteri Pendayagunaan Aparatur Negara dan Reformasi Birokrasi Nomor 90 Tahun 2021 tentang Pembangunan dan Evaluasi Zona Integritas Menuju Wilayah Bebas dari Korupsi dan Wilayah Birokrasi Bersih dan Melayani di Instansi Pemerintah.
- International Organization for Standardization. (2015). *ISO 9001:2015 Quality management systems — Requirements*.
- International Organization for Standardization. (2016). *ISO 37001:2016 Anti-bribery management systems — Requirements with guidance for use*.
- Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. *Journal of the American Statistical Association, 22*(158), 209–212.
