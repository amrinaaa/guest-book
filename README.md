# Aplikasi Buku Tamu Haskell

Aplikasi Buku Tamu sederhana yang diimplementasikan menggunakan bahasa pemrograman Haskell.

## Fitur

- Menambah tamu baru
- Melihat daftar tamu
- Mengedit informasi tamu
- Menghapus tamu
- Mencari tamu berdasarkan nama atau email
- Mengurutkan daftar tamu
- Melihat statistik tamu
- Mengekspor data tamu ke CSV

## Prasyarat

Sebelum menjalankan aplikasi ini, pastikan Anda telah menginstal:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/) atau [Stack](https://docs.haskellstack.org/en/stable/README/)

## Cara Menjalankan Aplikasi

### Menggunakan Cabal

1. Clone repositori ini:
git clone https://github.com/username/haskell-buku-tamu.git cd haskell-buku-tamu


2. Build proyek:

cabal build


3. Jalankan aplikasi:
cabal run


## Penggunaan

Setelah menjalankan aplikasi, Anda akan melihat menu utama dengan berbagai opsi. Gunakan nomor yang sesuai untuk memilih opsi yang diinginkan.

Contoh penggunaan:
1. Pilih opsi 1 untuk menambah tamu baru
2. Masukkan informasi tamu yang diminta (nama, email, nomor HP)
3. Pilih opsi 2 untuk melihat daftar tamu yang telah ditambahkan

## Struktur Proyek

- `main.hs`: File utama yang berisi logika aplikasi
- `main.cabal`: File konfigurasi Cabal
- `data_tamu.txt`: File untuk menyimpan data tamu (akan dibuat otomatis)
- `activity_log.txt`: File log aktivitas (akan dibuat otomatis)

## Kontribusi

Kontribusi selalu diterima. Silakan buat pull request atau laporkan issues jika Anda menemukan bug atau memiliki saran perbaikan.

## Lisensi

[MIT License](LICENSE)