package controllers

import play.api._
import play.api.mvc._

object SpecialWayController extends Controller {
  
    /**
     * Company Login Page
     */
    def swLoginIndex = Action {
        Ok(views.html.swLoginIndex("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginAccountExtra = Action {
        Ok(views.html.swLoginAccountExtra("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginAccountNormalInfo = Action {
        Ok(views.html.swLoginAccountNormalInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginAccountPeople = Action {
        Ok(views.html.swLoginAccountPeople("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginAccountPsw = Action {
        Ok(views.html.swLoginAccountPsw("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginAccountValidateInfo = Action {
        Ok(views.html.swLoginAccountValidateInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginCompanyList = Action {
        Ok(views.html.swLoginCompanyList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginCompleteInfo = Action {
        Ok(views.html.swLoginCompleteInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginCompleteProduct = Action {
        Ok(views.html.swLoginCompleteProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginDriverList = Action {
        Ok(views.html.swLoginDriverList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginRecruitment = Action {
        Ok(views.html.swLoginRecruitment("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginSendInfo = Action {
        Ok(views.html.swLoginSendInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginSendProduct = Action {
        Ok(views.html.swLoginSendProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginSentInfo = Action {
        Ok(views.html.swLoginSentInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginSentProduct = Action {
        Ok(views.html.swLoginSentProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginSpecialWayList = Action {
        Ok(views.html.swLoginSpecialWayList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def swLoginRewards = Action {
        Ok(views.html.swLoginRewards("Your new application is ready."))
    }


}