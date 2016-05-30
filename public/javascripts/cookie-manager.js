function setCookie(cname,cvalue,expiredays) {
    var exp  = new Date();
    exp.setTime(exp.getTime() + expiredays*24*60*60*1000);
    document.cookie = cname + "="+ escape(cvalue) + ";expires=" + exp.toGMTString()+";path="+"/";
}

function clearCookie(name) {  
    var exp = new Date();
    exp.setTime(exp.getTime() - 1);
    document.cookie= name + "=''"+";expires="+exp.toGMTString()+";path="+"/";
}  

function adminLogout() {
    clearCookie('user_id');
    clearCookie('token');
    clearCookie('screen_name');
    location.href = '/admin/login';
}

function logout() {
	clearCookie('user_id');
    clearCookie('token');
    clearCookie('screen_name');
    location.href = '/';
}