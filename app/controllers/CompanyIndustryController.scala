package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.companyOpt.companySearchModule
import module.common.xml.xmlOpt

object CompanyIndustryController extends Controller {
  
    /**
     * Company Login Page
     */
    def ciLoginIndex = Action {
        val dir_lst = (companySearchModule.queryDrivers(toJson("")) \ "result").asOpt[List[JsValue]].get
        Ok(views.html.ciLoginIndex(dir_lst))
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountExtra = Action {
        Ok(views.html.ciLoginAccountExtra("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountNormalInfo = Action {
        Ok(views.html.ciLoginAccountNormalInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountPeople = Action {
        Ok(views.html.ciLoginAccountPeople("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountProductName = Action {
        Ok(views.html.ciLoginAccountProductName("Your new application is ready."))
    }


    /**
     * Company Login Page
     */
    def ciLoginAccountPsw = Action {
        Ok(views.html.ciLoginAccountPsw("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountValidateInfo = Action {
        Ok(views.html.ciLoginAccountValidateInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginCompanyList = Action {
        Ok(views.html.ciLoginCompanyList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginCompleteInfo = Action {
        Ok(views.html.ciLoginCompleteInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginCompleteProduct = Action {
        Ok(views.html.ciLoginCompleteProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginDriverList = Action {
        Ok(views.html.ciLoginDriverList(xmlOpt.allCities))
    }

    /**
     * Company Login Page
     */
    def ciLoginRecruitment = Action {
        Ok(views.html.ciLoginRecruitment("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginSendInfo = Action {
        Ok(views.html.ciLoginSendInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginSendProduct = Action {
        Ok(views.html.ciLoginSendProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginSentInfo = Action {
        Ok(views.html.ciLoginSentInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginSentProduct = Action {
        Ok(views.html.ciLoginSentProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginSpecialWayList = Action {
        Ok(views.html.ciLoginSpecialWayList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginRewards = Action {
        Ok(views.html.ciLoginRewards("Your new application is ready."))
    }

    def companySearchDriver = Action (request => requestArgs(request)(companySearchModule.queryDrivers))
}