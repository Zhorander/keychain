open Cryptokit

let rng = Random.device_rng "/dev/urandom";;

let hex_encode str =
  transform_string (Hexa.encode ()) str;;

let hex_encode str =
  transform_string (Hexa.decode ()) str;;

(*output 32 bytes*)
let hash msg =
  let h_method = Hash.sha256 () in
  hash_string h_method msg;;

let enc_cipher key =
  let enc = new Block.aes_encrypt key in
  new Block.cipher_padded_encrypt Padding._8000 enc;;

let dec_cipher key =
  let dec = new Block.aes_decrypt key in
  new Block.cipher_padded_decrypt Padding._8000 dec;;

let rsa_key_gen rng =
  RSA.new_key ~rng:rng 1024;;

let rsa_sign key msg =
  RSA.sign key msg;;

let encrypt_msg cipher msg =
  transform_string cipher msg

let decrypt_msg = encrypt_msg;;

let enc msg key =
  let cipher = enc_cipher key in
  encrypt_msg cipher msg;;

let dec msg key =
  let cipher = dec_cipher key in
  decrypt_msg cipher msg;;

let wipe_str str =
  wipe_string str;