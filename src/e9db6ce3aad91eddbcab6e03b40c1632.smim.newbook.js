sap.ui.define([
    "zhr237/controller/Libs",
    "zhr237/controller/SchemaLayer",
    "sap/ui/base/Object",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
], function (Libs, SchemaLayer, Object, Filter, FilterOperator) {
    "use strict";

    return Object.extend("zhr237.controller.NewBook", {
        owner: null,
        _book: {
        },

        constructor: function (owner) {
            this.owner = owner

            this._model = new sap.ui.model.json.JSONModel()
            this._model.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            this._model.setData(this._book)

            this._dialog = sap.ui.xmlfragment("zhr237.fragment.NewBook", this)
            this._dialog.setModel(this._model, "book")
            this.owner.getView().addDependent(this._dialog)
        },

        showDialog: function (userInfo, previous_values) {
            this.userInfo = userInfo
            this.previous_values = previous_values

            Libs.copy_from(previous_values, this._book)

            this._book.minDate = Libs.get_date_from_now(userInfo.min_date)
            this._book.maxDate = Libs.get_date_from_now(userInfo.max_date)

            this._book.is_super_user = userInfo.is_manager || userInfo.is_admin

            this.layerCombobox = sap.ui.getCore().byId('id_new_layer_id')
            this.pernrCombobox = sap.ui.getCore().byId('id_new_book_pernr')
            this.desksCombobox = sap.ui.getCore().byId('id_new_book_desk')

            this.layerCombobox.setSelectedKey(null)
            this.desksCombobox.setSelectedKey(null)
            if (this._book.is_super_user)
                this.pernrCombobox.setSelectedKey(null)

            // Show what available
            // this.layerCombobox.getBinding("items").filter(new Filter("persa", FilterOperator.EQ, this.userInfo.persa))

            this._update_bindings()

            this._model.updateBindings()
            this._dialog.open()
        },

        _update_bindings: function () {
            //if (this._book.layer_address)
            this.pernrCombobox.getBinding("items").filter([new Filter("persa", FilterOperator.EQ, this._book.persa)])

            this.owner.getOwnerComponent().getModel().read("/ZC_HR237_Booking", {
                filters: this._make_default_filter(new Filter("datum", FilterOperator.EQ, Libs.getDateIso(Libs.get_noon(this._book.datum)))),

                urlParameters: {
                    "$select": "place_id" +
                        ",pernr,ename,user_name,created_when"  // Info about who already booked the desk
                },

                success: function (data) {
                    // if edit then show currently booked desk in combobox
                    if (this.previous_values._change)
                        data.results = data.results.filter(item => item.place_id !== this.previous_values.place_id)

                    this._skip_desks = data.results

                    const arrFilter = this._make_default_filter()
                    for (let item of this._skip_desks)
                        arrFilter.push(new Filter("place_id", FilterOperator.NE, item.place_id))

                    this.desksCombobox.getBinding("items").filter(arrFilter)
                }.bind(this)
            })
        },

        _make_default_filter: function (oFilter) {
            const result = oFilter ? [oFilter] : []

            if (this._book.layer_address)
                result.push(new Filter("layer_id", FilterOperator.EQ, this._book.layer_id))

            const selectedPernr = this.pernrCombobox.getSelectedItem()
            const department = selectedPernr ?
                selectedPernr.getBindingContext().getObject().department :
                this.userInfo.department
            result.push(new Filter("department", FilterOperator.EQ, department))

            return result
        },

        _on_schema_combobox_changed: function (oEvent) {
            if (!oEvent.getSource().getSelectedKey()) return

            const src = oEvent.getParameter('selectedItem').getBindingContext().getObject()
            this._book.layer_id = src.layer_id
            this._book.layer_text = src.layer_text
            this._book.layer_address = src.layer_address
            this._book.persa = src.persa
            this._book.name1 = src.name1
            this._update_bindings()
        },

        onDateChanged: function (oEvent) {
            if (oEvent && !oEvent.getParameter('valid')) return

            this._update_bindings()
        },


        handleActionButton: function () {
            const newBooking = {
                datum: Libs.get_noon(this._book.datum),
                pernr: this.pernrCombobox.getSelectedKey(),
                place_id: this.desksCombobox.getSelectedKey()
            }

            console.log(newBooking)
            console.log(this.desksCombobox.getValue())
            console.log(this.desksCombobox.getSelectedItem())

            if (!newBooking.pernr || !newBooking.place_id || !this._book.datum) {
                Libs.showMessage('Fill all required fields', true)
                return
            }

            if (this.previous_values._change) {
                newBooking.datum_prev = this.previous_values.datum
                newBooking.pernr_prev = this.previous_values.pernr
                newBooking.place_id_prev = this.previous_values.place_id
            }
            this.owner.getView().getModel().create(this._book._change ? '/ZC_HR237_QrCode' : '/ZC_HR237_Booking',
                newBooking,
                {
                    success: function (booking) {
                        Libs.send_request(Libs.get_qr_code_url(booking.datum, booking.pernr, 'NOTIFY'))

                        Libs.showMessage(
                            `Desk ${booking.place_text} booked for ${Libs.date_to_text(booking.datum)}\n` +
                            `Notification to ${booking.ename} via email is sent`)

                        this.owner.onStartDateChange()
                        this._dialog.close()
                    }.bind(this)
                })
        },

        onShowMap: function () {
            if (!this._book.layer_address) {
                Libs.showMessage('Please select schema', true)
                return
            }
            window.open(`https://www.google.com/maps/place/${encodeURIComponent(this._book.layer_address)}`, '_blank').focus()
        },

        onWhatchSchema: function () {
            if (!this._book.layer_address) {
                Libs.showMessage('Please select schema', true)
                return
            }

            sap.ui.core.BusyIndicator.show(0)
            if (!this.owner._schemaLayer)
                this.owner._schemaLayer = new SchemaLayer(this.owner)
            sap.ui.core.BusyIndicator.hide()

            const addFilter = this._make_default_filter()
            this.owner._schemaLayer.display(`${this._book.layer_id}^${this._book.layer_text}`, {
                addFilter: addFilter,
                skip_desks: this._skip_desks,
                callBack: function (place_id) {
                    this.desksCombobox.setSelectedKey(place_id)
                }.bind(this)
            })
        },

        handleEditCancelButton: function () {
            this._dialog.close()
        },

        initPernr: function(oEvent){
            sap.ui.getCore().byId("id_pernr").setValue(this._book.pernr)
            sap.ui.getCore().byId("id_pernr-input").setValue(this._book.pernr)
        }

    });
}
);