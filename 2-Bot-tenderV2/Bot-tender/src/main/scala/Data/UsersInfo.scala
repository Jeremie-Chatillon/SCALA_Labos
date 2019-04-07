package Data

import scala.collection.mutable

object UsersInfo {

  private var DEFAULT_CREDIT : Double = 30.0f
  // Will contain the name of the currently active user; default value is null.
  private var _activeUser: String = _

  // TODO: step 2 - create an attribute that will contain each user and its current balance.
  private var accounts : scala.collection.mutable.Map[String, Double]= scala.collection.mutable.Map()

  /**
    * Update an account by decreasing its balance.
    * @param user the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  // TODO: step 2
  def purchase(amount: Double, user: String = _activeUser): Double = {
    accounts(user) = accounts(user) - amount
    accounts(user)
  }

  def login(user: String): Unit = {
    if(!accounts.contains(user)){
      accounts += (user -> DEFAULT_CREDIT)
    }
    _activeUser = user
  }

  def userConnected() : String = _activeUser

  def getSolde() : Double = if(_activeUser != null) accounts(_activeUser) else 0.0f // TODO optimiser

  def logout() : Unit = _activeUser = null
}
