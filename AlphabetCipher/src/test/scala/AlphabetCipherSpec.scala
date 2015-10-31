import org.scalatest._

import AlphabetCipher.decipher
import AlphabetCipher.decode
import AlphabetCipher.encode
import AlphabetCipher.findShortestRepeat

class AlphabetCipherSpec extends FlatSpec with Matchers {
  "encode" should "encode given a secret keyword" in {
    assert(encode("a","k") == "k")
    assert(encode("i","e") == "m")
    assert(encode("v","m") == "h")
    assert(encode("vigilance","meetmeontuesdayeveningatseven") == "hmkbxebpxpmyllyrxiiqtoltfgzzv")
    assert(encode("scones","meetmebythetree") == "egsgqwtahuiljgs")
  }

  "decode" should "decode an cyrpted message given a secret keyword" in {
    assert(decode("a","k") == "k")
    assert(decode("i","m") == "e")
    assert(decode("v","h") == "m")
    assert(decode("vigilance","hmkbxebpxpmyllyrxiiqtoltfgzzv") == "meetmeontuesdayeveningatseven")
    assert(decode("scones","egsgqwtahuiljgs") == "meetmebythetree")

  }

  "decipher" should "extract the secret keyword given an encrypted message and the original message" in {
    assert(decipher("opkyfipmfmwcvqoklyhxywgeecpvhelzg", "thequickbrownfoxjumpsoveralazydog") == "vigilance")
    assert(decipher("hcqxqqtqljmlzhwiivgbsapaiwcenmyu", "packmyboxwithfivedozenliquorjugs") == "scones")
  }
  
  "findShortestRepeat" should "find the shorted repeated word" in {
    assert(findShortestRepeat("vigilance") == "vigilance")
    assert(findShortestRepeat("vigilancevigilance") == "vigilance")
    assert(findShortestRepeat("abcdabcdab") == "abcd")
  }
}
