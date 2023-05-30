sap.ui.define([
    "zhr237/controller/Libs",
    "sap/ui/model/odata/ODataMessageParser"], function (Libs, ODataMessageParser) {
        "use strict";

        return ODataMessageParser.extend("zhr237.ext.controller.MessageParser", {

            parse: function (oResponse, oRequest, mGetEntities, mChangeEntities, bMessageScopeSupported) {
                var _this = this
                ODataMessageParser.prototype.parse.apply(_this, arguments)

                if (oResponse.statusCode < 400 || oResponse.statusCode >= 600)
                    return

                const mRequestInfo = {
                    request: oRequest,
                    response: oResponse,
                    url: oRequest.requestUri
                }
                const aMessages = this._parseBody(oResponse, mRequestInfo)

                const allMessages = []
                const allTypes = []
                for (let msg of aMessages) {
                    if (msg.message.indexOf("An exception was raised") === -1 &&
                        msg.message.indexOf("In the context of Data Services an unknown internal server error occurred") === -1)
                        allMessages.push(msg.message)
                    allTypes.push(msg.type)
                }
                if (allMessages.length === 0)
                    return

                Libs.showMessage(allMessages.join('\n'), allTypes.indexOf("Error") >= 0)
            }

        });
    })