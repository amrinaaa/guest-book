{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Time
import System.IO
import Control.Monad
import System.Directory (doesFileExist)
import Data.List (intercalate, transpose, sortOn, isPrefixOf, tails, isInfixOf)
import Data.Char (toLower)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Calendar (toGregorian)
import qualified Data.Map.Strict as Map
import System.IO (appendFile)


data Tamu = Tamu
  { idTamu :: Int
  , namaTamu :: String
  , emailTamu :: String
  , noHpTamu :: String
  , checkInTime :: UTCTime
  , checkOutTime :: Maybe UTCTime
  , createdAtTamu :: UTCTime
  , updatedAtTamu :: UTCTime
  } deriving (Show, Read, Generic)

instance ToJSON Tamu
instance FromJSON Tamu

-- File operations
tulisTamuKeFile :: FilePath -> [Tamu] -> IO ()
tulisTamuKeFile file = B.writeFile file . encode

bacaTamuDariFile :: FilePath -> IO [Tamu]
bacaTamuDariFile file = do
    fileExist <- doesFileExist file
    if not fileExist
        then return []
        else maybe [] id . decode <$> B.readFile file

-- CRUD operations
tambahTamu :: FilePath -> [Tamu] -> Tamu -> String -> IO [Tamu]
tambahTamu file daftar tamu namaSeminar = do
    let daftarBaru = tamu : daftar
    tulisTamuKeFile file daftarBaru
    logActivity $ "Tamu ditambahkan: " ++ show (idTamu tamu)
    putStrLn $ "Tamu berhasil ditambahkan dalam Seminar " ++ namaSeminar
    return daftarBaru

editTamu :: FilePath -> [Tamu] -> Int -> (Tamu -> Tamu) -> String -> IO [Tamu]
editTamu file daftar id updateFunc namaSeminar= do
    let daftarBaru = map (\t -> if idTamu t == id 
                                then let tamuBaru = updateFunc t
                                    in tamuBaru { idTamu = idTamu t }
                                else t) daftar
    tulisTamuKeFile file daftarBaru
    logActivity $ "Tamu diubah: " ++ show id
    putStrLn $ "Tamu dengan ID " ++ show id ++ " berhasil diedit di seminar " ++ namaSeminar
    return daftarBaru

-- Fungsi untuk menghapus data tamu
hapusTamu :: FilePath -> [Tamu] -> Int -> String -> IO [Tamu]
hapusTamu file daftar id namaSeminar = do
    let daftarBaru = filter (\t -> idTamu t /= id) daftar
    tulisTamuKeFile file daftarBaru
    logActivity $ "Tamu dihapus: " ++ show id
    putStrLn $ "Tamu dengan ID " ++ show id ++ " telah dihapus dari seminar " ++ namaSeminar ++ "."
    return daftarBaru

-- Search and sort
cariTamu :: String -> [Tamu] -> [Tamu]
cariTamu query = filter (\t -> query `isInfixOf` map toLower (namaTamu t) || query `isInfixOf` map toLower (emailTamu t))
  where isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

sortTamu :: Ord a => (Tamu -> a) -> [Tamu] -> [Tamu]
sortTamu = sortOn

-- Statistics
tamuPerBulan :: [Tamu] -> Map.Map (Integer, Int) Int
tamuPerBulan = Map.fromListWith (+) . map (\t -> (toYearMonth (createdAtTamu t), 1))
  where toYearMonth time = let (year, month, _) = toGregorian (utctDay time) in (year, month)

-- Export to CSV
exportToCSV :: FilePath -> [Tamu] -> IO ()
exportToCSV file tamus = writeFile file $ unlines $ 
    "ID,Nama,Email,No HP,Check-In,Check-Out,Created At,Updated At" : 
    map tamuToCSV tamus
  where
    tamuToCSV t = intercalate " | " 
        [ show (idTamu t)
        , namaTamu t
        , emailTamu t
        , noHpTamu t
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (checkInTime t)
        , maybe "Belum Check-Out" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (checkOutTime t)
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAtTamu t)
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (updatedAtTamu t)
        ]

-- Input validation
validateEmail :: String -> Bool
validateEmail email = '@' `elem` email && '.' `elem` (dropWhile (/= '@') email)

validatePhoneNumber :: String -> Bool
validatePhoneNumber = all (\c -> c `elem` ("0123456789+- " :: String))

-- Logging
logActivity :: String -> IO ()
logActivity activity = do
    time <- getCurrentTime
    appendFile "activity_log.txt" $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time ++ " - " ++ activity ++ "\n"

-- Table display
createTable :: [[String]] -> String
createTable rows =
    let columns = transpose rows
        columnWidths = map (maximum . map length) columns
        totalWidth = sum columnWidths + length columnWidths * 3 + 1
        separator = "+" ++ intercalate "+" (map (flip replicate '-' . (+2)) columnWidths) ++ "+"
        formatRow = intercalate "|" . zipWith (\w s -> " " ++ padRight w s ++ " ") columnWidths
        padRight n s = s ++ replicate (n - length s) ' '
        header = take totalWidth $ repeat '='
    in unlines $ 
        header : 
        formatRow (head rows) : 
        separator :
        map formatRow (tail rows) ++
        [separator, header]

lihatTamu :: [Tamu] -> IO ()
lihatTamu [] = putStrLn "Tidak ada tamu dalam daftar."
lihatTamu daftar = do
    -- adakutambah
    let headers = ["ID", "Nama", "Email", "No HP",  "Check-In", "Check-Out", "Created At", "Updated At"]
        rows = headers : map tamuToRow daftar
    putStrLn $ createTable rows
  where
    tamuToRow tamu = 
        [ show (idTamu tamu)
        , namaTamu tamu
        , emailTamu tamu
        , noHpTamu tamu
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (checkInTime tamu)
        , maybe "Belum Check-Out" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (checkOutTime tamu)
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAtTamu tamu)
        , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (updatedAtTamu tamu)
        ]

checkOutTamu :: FilePath -> [Tamu] -> Int -> IO [Tamu]
checkOutTamu file daftar id = do
    currentTime <- getCurrentTime
    let daftarBaru = map (updateCheckOut currentTime) daftar
    tulisTamuKeFile file daftarBaru
    logActivity $ "Tamu check-out: " ++ show id
    return daftarBaru
  where
    updateCheckOut currentTime tamu 
        | idTamu tamu == id = tamu { 
            checkOutTime = Just currentTime, 
            updatedAtTamu = currentTime 
          }
        | otherwise = tamu

-- Main program
main :: IO ()
main = do
    let fileTamu = "data_tamu.txt"
    daftarTamu <- bacaTamuDariFile fileTamu
    namaSeminar <- inputNamaSeminar
    mainLoop fileTamu daftarTamu namaSeminar

-- Meminta Nama Seminar dan Konfirmasi
inputNamaSeminar :: IO String
inputNamaSeminar = do
    putStrLn "Masukkan nama seminar: "
    namaSeminar <- getLine
    putStrLn $ "Apakah Anda yakin nama seminar adalah: " ++ namaSeminar ++ "?"
    putStrLn "1. Ya"
    putStrLn "2. Tidak"
    pilihan <- getLine
    case pilihan of
        "1" -> return namaSeminar
        "2" -> inputNamaSeminar  -- Jika tidak, minta ulang
        _   -> do
            putStrLn "Pilihan tidak valid. Coba lagi!"
            inputNamaSeminar  -- Jika input tidak valid, minta ulang

-- Main Loop yang menggunakan Nama Seminar
mainLoop :: FilePath -> [Tamu] -> String -> IO ()
mainLoop file daftar namaSeminar = do
    putStrLn $ "\nSeminar: " ++ namaSeminar
    putStrLn "\nAplikasi Buku Tamu"
    putStrLn "1. Tambah Tamu"
    putStrLn "2. Lihat Daftar Tamu"
    putStrLn "3. Edit Tamu"
    putStrLn "4. Hapus Tamu"
    putStrLn "5. Cari Tamu"
    putStrLn "6. Urutkan Daftar Tamu"
    putStrLn "7. Statistik Tamu"
    putStrLn "8. Export ke CSV"
    putStrLn "9. Check-Out Tamu" 
    putStrLn "10. Keluar" 
    putStrLn "Pilih menu: "
    pilihan <- getLine
    case pilihan of
        "1" -> do
            tamu <- inputTamu daftar
            daftarBaru <- tambahTamu file daftar tamu namaSeminar
            mainLoop file daftarBaru namaSeminar
        "2" -> do
            lihatTamu daftar
            mainLoop file daftar namaSeminar
        "3" -> do
            putStrLn "Masukkan ID tamu yang ingin diedit: "
            idStr <- getLine
            let id = read idStr :: Int
            tamu <- inputTamu daftar
            let updateFunc = const tamu
            daftarBaru <- editTamu file daftar id updateFunc namaSeminar
            mainLoop file daftarBaru namaSeminar

        "4" -> do
            putStrLn "Masukkan ID tamu yang ingin dihapus: "
            idStr <- getLine
            let id = read idStr :: Int
            putStrLn "Masukkan nama seminar tempat tamu ini hadir: "
            namaSeminar <- getLine
            daftarBaru <- hapusTamu file daftar id namaSeminar
            mainLoop file daftarBaru namaSeminar

        "5" -> do
            putStrLn "Masukkan kata kunci pencarian: "
            query <- getLine
            let hasil = cariTamu query daftar
            lihatTamu hasil
            mainLoop file daftar namaSeminar
        "6" -> do
            putStrLn "Urutkan berdasarkan:"
            putStrLn "1. Nama"
            putStrLn "2. Email"
            putStrLn "3. Tanggal Dibuat"
            kriteria <- getLine
            let daftarUrut = case kriteria of
                    "1" -> sortTamu namaTamu daftar
                    "2" -> sortTamu emailTamu daftar
                    "3" -> sortTamu createdAtTamu daftar
                    _   -> daftar
            lihatTamu daftarUrut
            mainLoop file daftar namaSeminar
        "7" -> do
            let stats = tamuPerBulan daftar
            putStrLn "Statistik Tamu per Bulan:"
            mapM_ (\((year, month), count) -> 
                putStrLn $ show year ++ "-" ++ show month ++ ": " ++ show count) 
                (Map.toList stats)
            mainLoop file daftar namaSeminar
        "8" -> do
            putStrLn "Masukkan nama file CSV untuk export: "
            csvFile <- getLine
            exportToCSV csvFile daftar
            putStrLn $ "Data telah diekspor ke " ++ csvFile
            mainLoop file daftar namaSeminar
        "9" -> do 
            putStrLn "Masukkan ID tamu yang ingin check-out: "
            idStr <- getLine
            let id = read idStr :: Int
            daftarBaru <- checkOutTamu file daftar id
            mainLoop file daftarBaru namaSeminar
        "10" -> putStrLn "Terima kasih telah menggunakan aplikasi Buku Tamu."
        _ -> do
            putStrLn "Pilihan tidak valid. Coba lagi!"
            mainLoop file daftar namaSeminar

inputTamu :: [Tamu] -> IO Tamu
inputTamu daftar = do
    putStrLn "Masukkan Nama Tamu: "
    nama <- getLine
    inputEmail nama daftar

inputEmail :: String -> [Tamu] -> IO Tamu
inputEmail nama daftar = do
    putStrLn "Masukkan Email: "
    email <- getLine
    if not (validateEmail email)
        then do
            putStrLn "Email tidak valid. Coba lagi."
            inputEmail nama daftar
        else if email `elem` map emailTamu daftar
            then do
                putStrLn "Tamu sudah terdaftar"
                inputEmail nama daftar
            else inputNoHp nama email daftar

inputNoHp :: String -> String -> [Tamu] -> IO Tamu
inputNoHp nama email daftar = do
    putStrLn "Masukkan Nomor HP: "
    noHp <- getLine
    if not (validatePhoneNumber noHp)
        then do
            putStrLn "Nomor HP tidak valid. Coba lagi."
            inputNoHp nama email daftar
        else do
            -- adakutambah
            currentTime <- getCurrentTime
            return Tamu 
                { idTamu = if null daftar then 1 else maximum (map idTamu daftar) + 1
                , namaTamu = nama
                , emailTamu = email
                , noHpTamu = noHp
                , checkInTime = currentTime
                , checkOutTime = Nothing
                , createdAtTamu = currentTime
                , updatedAtTamu = currentTime
                }