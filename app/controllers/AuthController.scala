package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.auth.authStatus
import module.auth.authTypes

object AuthController extends Controller {
	def register = Action (request => requestArgs(request)(AuthModule.register))
	def sendCode = Action (request => requestArgs(request)(AuthModule.sendCode))
	def login = Action (request => requestArgs(request)(AuthModule.login))
	def update = Action (request => requestArgs(request)(AuthModule.updateProfile))
	def admainLogin = Action (request => requestArgs(request)(AuthModule.adminLogin))
//  def changePwd = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AuthModule.updatePwd)(authStatus.statusBase.t)(authTypes.notAuth.t))
  def changePwd = Action (request => requestArgs(request)(AuthModule.updatePwd))

  def pushSubuser = Action (request => requestArgs(request)(AuthModule.pushSubuser))
  def popSubuser = Action (request => requestArgs(request)(AuthModule.popSubuser))
  def updateSubuser = Action (request => requestArgs(request)(AuthModule.updateSubuser))
  
	def driverRegister = Action (request => requestArgs(request)(AuthModule.driverRegister))
  def driverUpdate = Action (request => requestArgs(request)(AuthModule.updateDriverProfile))
  
  def regCodeCheck = Action (request => requestArgs(request)(AuthModule.regCodeCheck))
}