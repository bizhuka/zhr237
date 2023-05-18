sap.ui.define([
    "sap/ui/base/Object",
], function (Object) {
    "use strict";

    return Object.extend("zhr237.controller.Libs", {

        getDateIso: function (date) {
            const okDate = new Date(date.getTime() - (date.getTimezoneOffset() * 60 * 1000))
            return okDate.toISOString().split('T')[0]
        },

        date_to_text: function (date) {
            return date.toLocaleString('default', { month: 'long', day: 'numeric' })
        },

        copy_from: function (src, dest) {
            for (let oldKey in dest)
                if (dest.hasOwnProperty(oldKey))
                    delete dest[oldKey]

            for (let newKey in src)
                if (src.hasOwnProperty(newKey)){
                    //console.log(typeof src[newKey], newKey)
                    dest[newKey] = src[newKey]
                }
        },

        get_noon: function (date) {
            if (!date)
                return null
            date.setHours(12, 0, 0, 0)
            return date
        },

        get_date_from_now: function (days) {
            const result = this.get_noon(new Date())
            result.setDate(result.getDate() + days)
            return result
        },

        showMessage: function (message, error) {
            sap.m.MessageToast.show(message, { duration: 3500 })
            if (error)
                $(".sapMMessageToast").css("background", "#cc1919")
        },

        get_avatar_url: function (pernr, size) {
            const urlBig = `${document.location.origin}/sap/opu/odata/sap/ZC_PY000_REPORT_CDS/ZC_PY000_PernrPhoto(pernr='${pernr}')/$value`
            if (size) return `${urlBig}?$filter=${encodeURIComponent(`img_size eq ${size}`)}`
            return urlBig
        },

        get_qr_code_url: function (date, pernr, action) {
            return `${document.location.origin}/sap/opu/odata/sap/ZC_HR237_BOOKING_CDS/ZC_HR237_QrCode(datum=datetime'${this.getDateIso(date)}T00:00:00',pernr='${pernr}',action='${action}')/$value`
        },

        send_request: function (theUrl) {
            var xmlHttp = new XMLHttpRequest()
            // , callback
            // xmlHttp.onreadystatechange = function () {
            //     if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
            //         callback(xmlHttp.responseText);
            // }
            xmlHttp.open("GET", theUrl, true) // true for asynchronous 
            xmlHttp.send(null)
        },

        isNumeric: function (str) {
            if (typeof str != "string") return false // we only process strings!  
            return !isNaN(str) && // use type coercion to parse the _entirety_ of the string (`parseFloat` alone does not do this)...
                !isNaN(parseFloat(str)) // ...and ensure strings of whitespace fail
        },


        addBindingListener: function (oBindingInfo, sEventName, fHandler) {
            oBindingInfo.events = oBindingInfo.events || {};

            if (!oBindingInfo.events[sEventName]) {
                oBindingInfo.events[sEventName] = fHandler;
            } else {
                // Wrap the event handler of the other party to add our handler.
                var fOriginalHandler = oBindingInfo.events[sEventName];
                oBindingInfo.events[sEventName] = function () {
                    fHandler.apply(this, arguments);
                    fOriginalHandler.apply(this, arguments);
                };
            }
        },

    });
}
);