

object Crypto:
  import javax.crypto.Mac
  import javax.crypto.spec.SecretKeySpec

  def hash(key: String, bytes: Array[Byte]) =
    val secretKeySpec = SecretKeySpec(key.getBytes(), "HmacSHA256")
    val mac           = Mac.getInstance("HmacSHA256")
    mac.init(secretKeySpec)
    bytesToHex(mac.doFinal(bytes))
  end hash

  def bytesToHex(hash: Array[Byte]): String =
    val hexString = new StringBuilder(2 * hash.length)
    for i <- hash.indices do
      val hex = Integer.toHexString(0xff & hash(i))
      if hex.length() == 1 then hexString.append('0')
      hexString.append(hex)
    hexString.toString()
  end bytesToHex
end Crypto
