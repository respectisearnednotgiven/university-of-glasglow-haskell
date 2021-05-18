import Data.Char

-- is this a letter to be ciphered?
shouldCipher :: Char -> Bool
shouldCipher c = isLetter(c) && isAscii(c)

-- enciphers single char at a time
cipherChar :: Int -> Char -> Char
cipherChar shift c
    | shouldCipher c = chr(ord(c) + shift)
    | otherwise    = c

-- encipher a whole strong
cipher :: Int -> [Char] -> [Char]
cipher shift = map (cipherChar shift) 

decipher :: Int -> [Char] -> [Char]
decipher shift ciphertext = cipher (-shift) ciphertext

