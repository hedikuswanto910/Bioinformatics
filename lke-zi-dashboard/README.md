# Dashboard LKE Zona Integritas (R Shiny)

Sistem pengisian Lembar Kerja Evaluasi (LKE) Zona Integritas yang **terarah, terurut, tervalidasi, dan tersistematis**, dengan validasi dan penyusunan draf berbantuan Claude (opsional).

## Kenapa sistem ini dibuat — dan apa yang tidak bisa diselesaikannya

Pada data penilaian sebelumnya (lihat menu *Pelajaran Penilaian Lalu* setelah riwayat diimpor), sebagian besar butir dinilai lebih rendah oleh evaluator dibandingkan klaim unit. Alasan terbanyak di catatan TPI/TPSK: dokumen tidak ada/tidak relevan, cakupan belum menyeluruh, inovasi tidak terbukti, tindak lanjut tidak dibuktikan, dan kegiatan tidak berkala.

Jadi masalah utamanya bukan sekadar "salah unggah", tetapi **jawaban diklaim lebih tinggi daripada yang dibuktikan dokumen**. Dashboard ini memaksa setiap PIC melihat kriteria dan catatan evaluator sebelumnya, lalu memeriksa isiannya secara otomatis. Namun:

- Validasi otomatis berbasis aturan hanya menangkap **masalah bentuk** (narasi kosong, tautan rusak, tahun bukti, data jumlah tidak konsisten, klaim "berkala/seluruh/inovasi" tanpa kata pendukung). Pada uji dengan data riwayat, aturan ini menangkap sekitar 6 dari 10 butir yang kemudian diturunkan TPI, tetapi juga menandai butir yang ternyata tidak diturunkan. Aturan **tidak bisa menilai substansi dokumen**.
- Substansi hanya bisa dinilai oleh orang yang membaca dokumennya, atau oleh Claude bila PDF bukti diunggah. Hasil Claude adalah **pra-penilaian**, bukan nilai resmi.
- Tidak ada sistem yang bisa menaikkan nilai bila kegiatannya memang belum dilakukan. Fitur draf dokumen Claude **dilarang mengarang fakta**: dokumen bukti yang tidak sesuai kenyataan adalah pemalsuan, dan berisiko besar justru pada penilaian integritas.

## Fitur

| Menu | Fungsi |
|---|---|
| Beranda | Nilai klaim (rumus identik dengan Excel LKE), status butir, grafik klaim vs nilai TPI periode lalu per komponen |
| Isi LKE | Daftar butir dalam dropdown per **Zona 1-6** (sama dengan sheet Zona di Excel) + Hasil, dengan hitungan status per zona, kolom pencarian, kode butir + judul singkat (pertanyaan lengkap tampil saat kursor diarahkan). Pengisian butir berurutan (tombol Sebelumnya/Berikutnya), kriteria tiap pilihan, catatan TP PTN/TPSK/TPI periode lalu, validasi langsung saat mengetik, rincian jumlah → persentase otomatis, unggah PDF bukti, tombol Claude |
| Validasi | Semua butir berwarna merah/kuning/hijau; klik baris untuk langsung membuka butirnya |
| Pelajaran Penilaian Lalu | Butir yang diturunkan, alasan terbanyak, persentase penurunan per komponen (bahan pelatihan PIC) |
| Draf Dokumen (Claude) | Draf laporan monev, tindak lanjut, notulen, dan lain-lain dari fakta kegiatan nyata → .docx |
| Keterangan Kode | Cara membaca kode (mis. `P1.ii.c` = Pemenuhan, Zona 1, sub-komponen ii, butir c) dan tabel semua kode beserta pertanyaan lengkapnya |
| Prosedur Mutu (SOP) | Pemeriksaan daftar PM + PDF: nomor ganda, nama file vs nomor dokumen, versi vs nomor revisi, kop "Halaman x dari N", status upload |
| Ekspor | Lembar isian Excel (urutan = baris LKE) dan laporan validasi Word |
| Pengaturan | Tahun, impor isian/riwayat dari Excel, unggah logo & gambar banner header, status & biaya Claude, jejak perubahan |

Mesin nilai sudah diuji terhadap file LKE Excel asli: nilai Pengungkit untuk keempat kolom penilai (Unit, TP PTN, TPSK, TPI) **sama persis** dengan hasil rumus Excel. Satu perbedaan sengaja: Excel memakai `AVERAGE()` yang **mengabaikan** butir kosong, sedangkan dashboard menghitung butir kosong = 0 agar butir yang belum diisi tidak tersembunyi.

## Instalasi

Butuh R ≥ 4.1 (disarankan RStudio). Di konsol R:

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "readxl", "openxlsx", "httr2",
                   "jsonlite", "RSQLite", "DBI", "ggplot2", "pdftools", "officer", "testthat"))
```

## Menyiapkan data (sekali per periode)

Semua perintah dijalankan dari folder `lke-zi-dashboard`.

```bash
Rscript scripts/siapkan_data.R --riwayat "Indeks_Revisi_FK_UNHAS.xlsx"
```

```bash
Rscript scripts/siapkan_data.R --isian "Indeks_Revisi_FK_UNHAS.xlsx" --tahun 2027
```

- `--riwayat` menyimpan jawaban dan catatan evaluator (kolom TP PTN/TPSK/TPI) agar muncul di setiap butir.
- `--isian` mengisi isian periode baru dengan isian lama sebagai draf (butir yang sudah diisi tidak ditimpa). Keduanya juga tersedia di menu **Pengaturan**.
- `--rubrik` hanya diperlukan bila Kementerian mengubah template. Butuh file berformat Indeks Revisi, yaitu yang kolom A-nya berisi penanda `PARENT`/`SUB`.

Data hasil impor disimpan di `data/lokal/`, dan folder ini **tidak ikut ke GitHub**.

## Menjalankan

```r
shiny::runApp("lke-zi-dashboard")
```

Tahun penilaian dan nama unit diatur lewat `.Renviron` (lihat `.Renviron.contoh`) atau di menu Pengaturan.

## Menghubungkan Claude

1. Langganan Claude.ai (Pro/Team) **tidak bisa** disambungkan ke aplikasi lain. Yang dibutuhkan adalah **API key** dari <https://console.anthropic.com>, dengan tagihan terpisah per pemakaian. Pengadaannya sebaiknya lewat akun institusi.
2. Salin `.Renviron.contoh` menjadi `.Renviron` di folder ini, lalu isi `ANTHROPIC_API_KEY=...`.
3. Mulai ulang R. Menu Pengaturan akan menampilkan "Claude aktif".

Model bawaan: `claude-opus-5-5` (bisa diganti lewat `ZI_CLAUDE_MODEL`). Bila filter keamanan menolak permintaan, server otomatis mencoba model cadangan (`fallbacks: "default"`).

**Perkiraan biaya** (estimasi, tarif US$4 per 1 juta token masukan dan US$20 per 1 juta token keluaran): satu pra-penilaian tanpa PDF sekitar 3–6 ribu token (≈ US$0,02–0,06). Dengan PDF, setiap halaman menambah sekitar 1,5–3 ribu token, sehingga 30 halaman ≈ US$0,25–0,45 per butir. Pemakaian aktual tercatat di menu Pengaturan.

**Privasi**: narasi dan PDF yang dikirim ke Claude diproses oleh Anthropic. Jangan kirim dokumen berisi data pribadi sensitif (NIK, data kesehatan, rincian LHKPN). Mintalah persetujuan pimpinan/unit TI sebelum dipakai.

## Aturan validasi otomatis

| Kode | Tingkat | Aturan |
|---|---|---|
| V01–V02 | merah | Jawaban/rincian/nilai survei kosong atau di luar rentang |
| V03 | merah | Penyebut persentase = 0 (di Excel menjadi `#DIV/0!` dan merusak total Pengungkit) |
| V04 | kuning | Pembilang > penyebut (nilai dipotong 100%) |
| V05 | merah | Jumlah wajib lapor LHKPN/LHKASN ≠ jumlah rinciannya |
| V06 | kuning | Data pelanggaran/pengaduan tidak logis |
| V07 | merah | Rincian berlabel sama diisi berbeda antarbutir (mis. jumlah perubahan agen perubahan) |
| V10 | merah/kuning | Narasi kosong / kurang dari 300 karakter |
| V11–V12 | merah/kuning | Bukti kosong, tautan tidak valid, judul dokumen terlalu umum |
| V13 | kuning | Tidak ada bukti bertahun periode berjalan |
| V20 | kuning | Kriteria jawaban menuntut "berkala/seluruh/inovasi/tindak lanjut/pimpinan/…" tetapi narasi dan judul bukti tidak menunjukkannya |
| V30 | info | Catatan evaluator periode lalu bila butir ini diturunkan |
| V31 | kuning | Masih mengklaim nilai tinggi dengan narasi yang sama persis seperti saat diturunkan |

Ambang angka dapat diubah di `R/00_konfigurasi.R`.

## Pemakaian bersama (banyak PIC)

- Aplikasi ini **belum memiliki login**. Siapa pun yang bisa membuka alamatnya bisa mengubah isian. Untuk pemakaian bersama, pasang di server kampus (Shiny Server / Posit Connect) di balik login SSO atau VPN.
- Setiap simpanan mencatat nama PIC dan waktunya (menu Pengaturan → Jejak perubahan).
- SQLite (`data/lokal/zi.sqlite`) cukup untuk tim kecil. Cadangkan file ini secara rutin.
- Tombol Claude menahan sesi pengguna tersebut selama 30–90 detik saat menunggu jawaban.

## Struktur

```
app.R                  antarmuka & alur Shiny
R/00_konfigurasi.R     pengaturan (tahun, model, ambang)
R/10_rubrik.R          rumus nilai (identik Excel)
R/20_validasi.R        aturan validasi
R/30_claude.R          integrasi Claude (httr2, keluaran JSON terstruktur)
R/40_penyimpanan.R     SQLite + jejak + log AI
R/50_ekspor.R          Excel & Word
R/60_sop.R             pemeriksa daftar PM & PDF SOP
R/70_impor.R           pembaca template LKE Excel
R/80_tampilan.R        tema warna, CSS, header & ribbon (ubah daftar WARNA untuk ganti tema)
www/gambar/            logo & banner bawaan (www/kustom/ = unggahan Anda, tidak di-commit)
data/rubrik_lke.csv    113 butir + 24 rincian, kriteria & bobot (publik)
data/lokal/            data internal unit (TIDAK di-commit)
scripts/siapkan_data.R impor dari Excel
tests/testthat/        uji otomatis
```

Uji otomatis:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```
