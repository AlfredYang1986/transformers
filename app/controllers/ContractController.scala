package controllers

import play.api._
import play.api.mvc._

object ContractController extends Controller{
    /**
     * driver contract
     */
    def driverContract = Action {
        Ok(views.html.driverContract("test"))
    }
    /**
     * company contract
     */
    def companyContract = Action {
        Ok(views.html.companyContract("test"))
    }
}