package Chat

object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
  // Operators
  val ET: Token          = 4
  val OU: Token          = 5
  // Products
  val BIERE: Token       = 6
  val CROISSANT: Token   = 7
  // Utils
  val PSEUDO: Token      = 9
  val NUM: Token         = 10
  val UNKNOWN: Token     = 11
  val EOL: Token         = 12
  // States
  val ASSOIFFE : Token = 13
  val AFFAME : Token = 14
  // BIERE
  val MARQUE : Token = 15
  // CONNECTION
  val APPELLER : Token = 16
  // COMMANDE
  val COMMANDER : Token = 17
  // SOLDE
  val CONNAITRE : Token = 18
  val SOLDE : Token = 19
  // PRIX
  val COUTER : Token = 20
  val COMBIEN : Token = 21
  val PRIX : Token = 22
  // Terms
  val MON : Token = 23
  val LE : Token = 24
  val ME : Token = 25
  val DE : Token = 26
  val QUEL : Token = 27
  val AUREVOIR: Token = 28
}
