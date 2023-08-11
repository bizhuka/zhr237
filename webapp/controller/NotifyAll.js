sap.ui.define([
    "zhr237/controller/Libs",
], function (Libs) {
    "use strict";

    return {

        init_notif_tab: function (owner) {
            this.owner = owner
            owner.getView().byId('idNotifyAllTable').rebindTable()
        },

        onBeforeRebindNotifyAllTable: function (oEvent) {
            const oBindingParams = oEvent.getParameter("bindingParams")
            //oBindingParams.parameters = oBindingParams.parameters || {}

            if (this._is_not_notified())
                oBindingParams.filters.push(new sap.ui.model.Filter("is_notified", "EQ", false))
        },

        _is_not_notified: function () {
            return !this.owner.getView().byId('id_is_notified').getState()
        },

        send: function () {

            const filter = this.owner.getView().byId("idNotifyAllTableInner").getBinding("items").sFilterParams

            //const url = `${document.location.origin}/sap/opu/odata/sap/ZC_HR237_BOOKING_CDS/ZC_HR237_A_Send_Notif_All?` + 
            // Libs.send_request(url, function () { }.bind(this))
            
            this.owner.getView().getModel().read("/ZC_HR237_A_Send_Notif_All", {
                urlParameters: {
                    //"$select": "",
                    "$filter": decodeURIComponent(filter).replace('$filter=', '') + ` and action ne 'NOTIFY_ALL'`
                },
                success: function () {
                    Libs.showMessage(`All employees have been notified`)
                    this.init_notif_tab(this.owner)
                }.bind(this),

                // error: function (oError) {
                //     console.log(oError)
                // }
            })
        }
    };
});