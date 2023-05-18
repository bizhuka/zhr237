sap.ui.define([
    "sap/ui/base/Object"
], function (Object) {
    "use strict";

    return Object.extend("zhr237.controller.FormLayer", {
        owner: null,
        _layer: {
            isNew: false,
            title: ""
        },

        constructor: function (owner) {
            this.owner = owner
            this._model = new sap.ui.model.json.JSONModel()
            this._model.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            this._model.setData(this._layer)

            this._dialog = sap.ui.xmlfragment("zhr237.fragment.FormLayer", this)
            this._dialog.setModel(this._model, "layer")
            this.owner.getView().addDependent(this._dialog)
        },

        popup: function (is_new, param_str, callBack) {
            const arrParams = param_str.split('^')

            this._layer.layer_id = is_new ? '' : arrParams[0]
            this._layer.layer_text = is_new ? '' : arrParams[1]
            this._layer.layer_address = is_new ? '' : arrParams[2]
            this._layer.persa = is_new ? '' : arrParams[3]

            this._layer.isNew = !this._layer.layer_id
            this._layer.title = this._layer.isNew ? "New layer" : "Edit layer"
            
            this.callBack = callBack

            this._model.updateBindings()
            this._dialog.open()
        },

        handleActionButton: function () {
            delete this._layer.isNew
            delete this._layer.title

            this.callBack(this._layer)
            this._dialog.close()
        },


        handleEditCancelButton: function () {
            this._dialog.close()
        },

    });
}
);