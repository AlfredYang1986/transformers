package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import controllers.common.requestArgsQuery.requestArgs
import module.auth.authTypes
import module.auth.registerTypes
import module.auth.authStatus
import module.applications.AppModule

object ApplicationController {
    def pushApplication = Action (request => requestArgs(request)(AppModule.pushApplication))
    def popApplication = Action (request => requestArgs(request)(AppModule.popApplication))
    def approveApplication = Action (request => requestArgs(request)(AppModule.approveApplication))
    def rejectApplication = Action (request => requestArgs(request)(AppModule.rejectApplication))
    def queryApplications = Action (request => requestArgs(request)(AppModule.queryApplications))
    def queryApplicationDetail = Action (request => requestArgs(request)(AppModule.queryApplicationDetail))
}