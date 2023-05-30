sap.ui.define([
    "zhr237/controller/Libs",
    "sap/ui/base/Object",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
], function (Libs, Object, Filter, FilterOperator) {
    "use strict";

    return Object.extend("zhr237.controller.DeskEdit", {
        owner: null,
        _desk: {
        },

        constructor: function (owner) {
            this.owner = owner

            this._model = new sap.ui.model.json.JSONModel()
            this._model.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            this._model.setData(this._desk)

            this._dialog = sap.ui.xmlfragment("zhr237.fragment.DeskEdit", this)
            this._dialog.setModel(this._model, "desk")
            this.owner.getView().addDependent(this._dialog)
        },

        showDialog: function (layer_id, callBack) {
            this.layer_id = layer_id
            this._desk.place_text = ''
            this._desk.department = ''
            this.callBack = callBack

            this.deskComboBox = sap.ui.getCore().byId('id_desk_combobox')
            this.deskComboBox.getBinding("items").filter([
                new Filter("layer_id", FilterOperator.EQ, this.layer_id)])

            this._model.updateBindings()
            this._dialog.open()
        },

        onSelectionChange: function (oEvent) {
            const selectedItem = oEvent.getParameter('selectedItem')
            if (selectedItem)
                this._desk.department = selectedItem.getBindingContext().getObject().department
        },

        handleActionButton: function (action) {
            const selectedKey = this.deskComboBox.getSelectedKey()
            const place_text = selectedKey ? selectedKey : this.deskComboBox.getValue()
            const place_id = `${this.layer_id}-${place_text}`

            if (!place_id || !place_id.match(/^[\s\S][\s\S]-\d+-\d+$/)) {
                Libs.showMessage(`Desk number '${place_id}' is invalid`, true)
                return
            }

            if (this._desk.department === '') {
                Libs.showMessage(`Please choose department`, true)
                return
            }

            const is_new = selectedKey === ''
            if (is_new && action === 'SAVE')
                Libs.showMessage(`Please set position for desk '${place_id}'`)
            action = is_new ? 'SET' : action

            this.callBack(action, is_new, {
                place_id: place_id,
                department: this._desk.department,

                place_text: place_text,
                orgtx: sap.ui.getCore().byId('id_desk_department').getSelectedItem().getBindingContext().getObject().orgtx
            })
            this._dialog.close()
        },


        handleEditCancelButton: function () {
            this._dialog.close()
        },

    });
}
);