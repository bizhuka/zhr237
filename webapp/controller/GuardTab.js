sap.ui.define([
    "zhr237/controller/Libs",
    "sap/ui/base/Object",
    "sap/m/Dialog",
    "sap/m/Button",
    "sap/m/Image",
    "sap/m/HBox"
], function (Libs, Object, Dialog, Button, Image, HBox) {
    "use strict";

    return Object.extend("zhr237.controller.GuardTab", {
        owner: null,

        constructor: function (owner) {
            this.owner = owner
            this.libs = new Libs()
            const _libs = this.libs

            owner.byId('idCheckBookingTable').attachBeforeRebindTable(function (oEvent) {
                const oBindingInfo = oEvent.getParameter("bindingParams")
                this.libs.addBindingListener(oBindingInfo, "dataReceived", this.onDataReceived.bind(this));
            }.bind(this))

            this.pernrFilter = owner.byId('idCheckBookingFilter-filterItemControl_BASIC-pernr')
            this.basicFilter = owner.byId('idCheckBookingFilter-btnBasicSearch')

            const datumFilter = owner.byId('idCheckBookingFilter-filterItemControl_BASIC-datum')
            const filterBar = owner.byId('idCheckBookingFilter')
            
            datumFilter.setValueFormat('yyyy-MM-dd')
            datumFilter.setValueState('None')

            function set_date_filter(datum) {
                datumFilter.setValue(_libs.getDateIso(_libs.get_noon(datum)))
            }
            function fireSearchBooking() {
                filterBar.fireSearch()
            }
            set_date_filter(new Date())
            fireSearchBooking()

            sap.ui.require(["sap/ndc/BarcodeScannerButton"], function (BarcodeScannerButton) {
                const toolbar = this.owner.byId('idCheckBookingToolbar')
                toolbar.addContent(new BarcodeScannerButton(
                    {
                        // id:"id_qr_scanner",
                        dialogTitle: "Barcode Scanner",
                        keepCameraScan: true,

                        scanFail: function (oEvent) {
                            this.libs.showMessage("Scan failed: " + oEvent, true)
                        },

                        scanSuccess: function (oEvent) {
                            if (oEvent.getParameter("cancelled")) {
                                this.libs.showMessage("Scan cancelled", true)
                                return
                            }

                            const qr_code = oEvent.getParameter("text")
                            const arrParts = qr_code.split('-')

                            if (qr_code.length !== 17 ||
                                !this.libs.isNumeric(qr_code.replace('-', '')) ||
                                arrParts.length !== 2 || arrParts[0].length !== 8 || arrParts[1].length !== 8) {
                                this.libs.showMessage(`QR code ${qr_code} is invalid`, true)
                                return
                            }

                            this.basicFilter.setValue(arrParts[1])
                            set_date_filter(new Date(parseInt(arrParts[0].substr(0, 4)), parseInt(arrParts[0].substr(4, 2)) - 1, parseInt(arrParts[0].substr(6, 2))))
                            fireSearchBooking()
                        }.bind(this)
                    }))
            }.bind(this))
        },

        onDataReceived: function (oEvent) {
            const bookings = oEvent.getParameter('data').results
            if (bookings.length !== 1 || ( !this.basicFilter.getValue() && !this.pernrFilter.getValue() )) return
            const booking = bookings[0]

            if (!this._avatarDialog)
                this._avatarDialog = new Dialog({
                    endButton: new Button({
                        text: 'Cancel',
                        press: function () {
                            this._avatarDialog.close()
                        }.bind(this)
                    }),

                    content: new HBox({
                        justifyContent: "Center",
                        items: new Image()
                    })

                })
            this._avatarDialog.setTitle(`${booking.ename} - ${booking.place_text} - ${this.libs.date_to_text(booking.datum)}`)
            this._avatarDialog.getContent()[0].getItems()[0].setSrc(this.libs.get_avatar_url(booking.pernr, 400))
            this._avatarDialog.open()
        }
    });
}
);