sap.ui.define([
    "zhr237/controller/Libs",
    "zhr237/controller/NewBook",
    "sap/ui/base/Object"
], function (Libs, NewBook, Object) {
    "use strict";

    return Object.extend("zhr237.controller.Details", {
        owner: null,
        _detail: {},

        constructor: function (owner) {
            this.owner = owner
            this.libs = new Libs()

            this._model = new sap.ui.model.json.JSONModel()
            this._model.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            this._model.setData(this._detail)

            this._popup = sap.ui.xmlfragment("zhr237.fragment.Details", this)
            this._popup.setModel(this._model, "detail")
            this.owner.getView().addDependent(this._popup)
        },

        showPopup: function (oAppointment) {
            const src = oAppointment.getBindingContext('calendar').getObject()._src
            this.libs.copy_from(src, this._detail)
            this._detail._update_is_active =
                this.libs.getDateIso(this.libs.get_noon(this._detail.datum))
                >= this.libs.getDateIso(this.libs.get_noon(new Date()))

            this._model.updateBindings()
            this._popup.openBy(oAppointment);
        },

        handleEditButton: function () {
            if (!this.owner._NewBook)
                this.owner._NewBook = new NewBook(this.owner)

            this.owner._NewBook.showDialog(this.owner.userInfo, {
                pernr: this._detail.pernr,
                datum: this._detail.datum,
                layer_id: this._detail.layer_id,
                persa: this._detail.persa,
                layer_text: this._detail.layer_text,
                name1: this._detail.name1,
                place_id: this._detail.place_id,
                _edit_callback: this.refresh_calendar.bind(this)
            })
        },

        handleEditCancelButton: function () {
            this._dialog.close()
        },

        refresh_calendar: function () {
            this.owner.onStartDateChange()
        },

        handlePopoverDeleteOrCancel: function () {
            if (!this._detail._update_is_active) {
                this._popup.close()
                return
            }

            const _detail = this._detail
            if (!this.oConfirmDialog) {
                this.oConfirmDialog = new sap.m.Dialog({
                    type: sap.m.DialogType.Message,
                    title: "Confirm",
                    content: new sap.m.Text({ text: "Do you want to delete this booking?" }),
                    beginButton: new sap.m.Button({
                        type: sap.m.ButtonType.Emphasized,
                        text: "Delete",
                        press: function () {
                            this.oConfirmDialog.close()

                            this.owner.getView().getModel().remove(`/ZC_HR237_Booking(datum=datetime'${this.libs.getDateIso(_detail.datum)}T00%3A00%3A00',pernr='${_detail.pernr}')`,
                                {
                                    success: function () {
                                        this.libs.showMessage(`Item ${_detail.place_id} for ${_detail.ename} at ${this.libs.date_to_text(_detail.datum)} was successfully deleted`)
                                        this.refresh_calendar()
                                    }.bind(this)
                                })
                        }.bind(this)
                    }),
                    endButton: new sap.m.Button({
                        text: "Cancel",
                        press: function () {
                            this.oConfirmDialog.close()
                        }.bind(this)
                    })
                });
            }

            this.oConfirmDialog.open()
        },

    });
}
);