object AlphabetCipher {
  private final val Alphabet = "abcdefghijklmnopqrstuvwxyz"
  
  def encode(keyword: String, message: String): String = { //TODO: can this be done without the for loop?
    var encoded = ""
    for(i <-0 until message.length){
     val m = Alphabet.indexOf(message.charAt(i))
     val k = Alphabet.indexOf(keyword.charAt(i%keyword.length()))
     val e = (m+k)%Alphabet.length()
     encoded += Alphabet.charAt(e)     
    }
    encoded
  }

  def decode(keyword: String, message: String): String = { //TODO: can this be done without the for loop?
    var decoded = ""
    for(i <-0 until message.length){
     val m = Alphabet.indexOf(message.charAt(i))
     val k = Alphabet.indexOf(keyword.charAt(i%keyword.length()))
     val e =((m - k) + 26 )%Alphabet.length()
     decoded += Alphabet.charAt(e)     
    }
    decoded
  }
  
  def decipher(cipher: String, message: String): String = {
    var deciphered = ""
    for(i <-0 until message.length){
     val m = Alphabet.indexOf(message.charAt(i))
     val e  =Alphabet.indexOf(cipher.charAt(i))
     val k = ((e - m) + 26 )%Alphabet.length()
     deciphered += Alphabet.charAt(k)     
    }
    findShortestRepeat(deciphered)
  }
  def findShortestRepeat(repeats : String): String = {
    ???
    //TODO: this is similar to a problem from the bioinformatics algorithm class you took
  }

}