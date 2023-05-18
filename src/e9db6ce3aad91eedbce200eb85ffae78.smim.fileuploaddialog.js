sap.ui.define([
    "zhr237/controller/Libs",
    "sap/ui/base/Object",
], function (Libs, Object) {
    "use strict";

    return Object.extend("zhr237.controller.FileUploadDialog", {
        owner: null,
        dialog: null,

        constructor: function (owner) {
            this.owner = owner
            this.libs = new Libs()

            this.dialog = sap.ui.xmlfragment("zhr237.fragment.FileUploadDialog", this);
            owner.getView().addDependent(this.dialog);
        },

        show: function (layer_id) {
            this.layer_id = layer_id
            this.dialog.open()
        },

        handleCancelPress: function (oEvent) {
            this.dialog.close();
            //this.dialog.destroy();
            //this.dialog = null;
        },

        handleUploadComplete: function (oEvent) {
            //var oResourceBundle = this.getView().getModel("i18n").getResourceBundle();
            var oResponse = oEvent.getParameters("response");
            const xmlDoc = new DOMParser().parseFromString(oResponse.responseRaw, "text/xml")

            let has_error = false
            let sMsg = ''
            try {
                sMsg = xmlDoc.getElementsByTagName('d:message')[0].childNodes[0].nodeValue
                has_error = xmlDoc.getElementsByTagName('d:is_error')[0].childNodes[0].nodeValue === 'X'
            } catch (error) {
                sMsg = xmlDoc.getElementsByTagName('message')[0].childNodes[0].nodeValue
                has_error = true
            }

            // refresh something

            this.libs.showMessage(sMsg, has_error)
        },

        handleUploadPress: function () {
            //perform upload
            //var oModel = this.getView().getModel();
            //var oResourceBundle = this.getView().getModel("i18n").getResourceBundle();
            var oFileUploader = sap.ui.getCore().byId("fupImport");
            var sMsg = "";

            //check file has been entered
            var sFile = oFileUploader.getValue();
            if (!sFile) {
                sMsg = "Please select a file first";
                sap.m.MessageToast.show(sMsg);
                return;
            }

            this._addTokenToUploader()
            oFileUploader.upload();
            this.dialog.close();
        },

        _addTokenToUploader: function () {
            //Add header parameters to file uploader.
            var oDataModel = this.owner.getView().getModel();
            var sTokenForUpload = oDataModel.getSecurityToken();
            var oFileUploader = sap.ui.getCore().byId("fupImport");
            var oHeaderParameter = new sap.ui.unified.FileUploaderParameter({
                name: "X-CSRF-Token",
                value: sTokenForUpload
            });

            var oHeaderSlug = new sap.ui.unified.FileUploaderParameter({
                name: "SLUG",
                value: this.layer_id + "|" +
                    encodeURIComponent(oFileUploader.getValue())
            });

            //Header parameter need to be removed then added.
            oFileUploader.removeAllHeaderParameters();
            oFileUploader.addHeaderParameter(oHeaderParameter);

            oFileUploader.addHeaderParameter(oHeaderSlug);
            //set upload url
            var sUrl = oDataModel.sServiceUrl + "/ZC_HR237_Layer";
            oFileUploader.setUploadUrl(sUrl);
        }
    });
}
);