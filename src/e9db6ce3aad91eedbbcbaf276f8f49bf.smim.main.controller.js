sap.ui.define([
    "zhr237/controller/Libs",
    "sap/ui/core/mvc/Controller",
    "zhr237/controller/SchemaLayer",
    "zhr237/controller/FormLayer",
    // "zhr237/controller/FormRoom",
    // "zhr237/controller/FormPlace",
    "zhr237/controller/NewBook",
    "zhr237/controller/CalendarBookingOverview",
    "zhr237/controller/NotifyAll",
],
    function (Libs, Controller, SchemaLayer, FormLayer, NewBook, CalendarBookingOverview, NotifyAll) {
        "use strict";

        return Controller.extend("zhr237.controller.Main", {

            onInit: function () {
                this._setMessageParser()

                this.mainView = this.byId("main_view")
                this.mainView.bindElement({
                    path: "/ZC_HR237_CUR_USER('')",

                    events: {
                        dataReceived: function () {
                            this.userInfo = this.mainView.getBindingContext().getObject()

                            this.byId("avatar_big").setSrc(Libs.get_avatar_url(this.userInfo.pernr))
                            this.byId("avatar_small").setSrc(Libs.get_avatar_url(this.userInfo.pernr), 64)
                        }.bind(this)
                    }
                })

                this.calendarBookingOverview = new CalendarBookingOverview(this)
            },


            onPick1Desk: function (oEvent) {
                const pickDeskButton = oEvent.getSource()
                pickDeskButton.setPressed(false)
                this.pickedDesk = null

                if (pickDeskButton.getText()) {
                    pickDeskButton.setText('')
                    this.onStartDateChange()
                    return
                }

                this.onBookClick({
                    pickModeCallback: function (pickedDesk) {
                        pickDeskButton.setPressed(true)
                        pickDeskButton.setText(pickedDesk.place_id)

                        this.pickedDesk = pickedDesk

                        this.onStartDateChange()
                        Libs.showMessage(`Click on day in calendar to set ${pickedDesk.place_id} desk`)
                    }.bind(this)
                })
            },

            handleIntervalSelect: function (oEvent) {
                if (!this.pickedDesk)
                    return
                const item = oEvent.mParameters.row.getBindingContext('calendar').getObject()

                const newBooking = {
                    datum: Libs.get_middle(oEvent.mParameters.startDate, oEvent.mParameters.endDate),
                    place_id: this.pickedDesk.place_id,
                    pernr: item.pernr,
                }

                this.createOrEdit(newBooking)
            },

            createOrEdit: function (newBooking, edit, callBack) {
                this.getView().getModel().create(edit ? '/ZC_HR237_A_Edit_Booking' : '/ZC_HR237_Booking',
                    newBooking,
                    {
                        success: function (booking) {
                            const sendNotif = !this.userInfo.is_manager
                            if (sendNotif)
                                Libs.send_request(Libs.get_base_url() + `ZC_HR237_A_Send_Notif(datum=datetime'${Libs.getDateIso(booking.datum)}T00:00:00',pernr='${booking.pernr}')/$value`)

                            Libs.showMessage(
                                `Desk ${booking.place_text} booked for ${Libs.date_to_text(booking.datum)}` +
                                (sendNotif ? `\nNotification to ${booking.ename} via email is sent` : ''))

                            this.onStartDateChange()

                            if (callBack)
                                callBack()
                        }.bind(this)
                    })
            },

            onSectionChange: function (oEvent) {
                switch (oEvent.getParameter('section')) {

                    case this.byId('bookingTab'):
                        this.onStartDateChange() //this.byId("bookingCalendar-Header-NavToolbar-TodayBtn").firePress()
                        return
                    case this.byId('guardTab'):
                        this._init_guard_tab()
                        return
                    case this.byId('chartTab'):
                        this._init_chart_tab()
                        return
                    case this.byId('notifTab'):
                        NotifyAll.init_notif_tab(this)
                        return
                }
            },

            onNotifyAll: function () {
                NotifyAll.send(this)
            },

            showScheduleReport: function () {
                this.calendarBookingOverview.showScheduleReport()
            },

            onBeforeRebindNotifyAllTable: function (oEvent) {
                NotifyAll.onBeforeRebindNotifyAllTable(oEvent)
            },

            onStartDateChange: function () {
                this.calendarBookingOverview.refresh_calendar()
                this.mainView.getObjectBinding().refresh()
            },

            _init_guard_tab: function () {
                sap.ui.core.BusyIndicator.show(0)
                sap.ui.require(["zhr237/controller/GuardTab"], function (GuardTab) {
                    sap.ui.core.BusyIndicator.hide()
                    if (!this._GuardTab)
                        this._GuardTab = new GuardTab(this)
                }.bind(this));
            },

            _init_chart_tab: function () {
                sap.ui.core.BusyIndicator.show(0)
                sap.ui.require(["zhr237/controller/ChartTab"], function (ChartTab) {
                    sap.ui.core.BusyIndicator.hide()
                    if (!this._ChartTab)
                        this._ChartTab = new ChartTab(this)
                }.bind(this));
            },

            uploadImage: function (oEvent) {
                const layer_id = oEvent.getSource().getProperty('target')
                sap.ui.require(["zhr237/controller/FileUploadDialog"], function (FileUploadDialog) {
                    if (!this._fileUploadDialog)
                        this._fileUploadDialog = new FileUploadDialog(this)
                    this._fileUploadDialog.show(layer_id)
                }.bind(this));
            },

            _setMessageParser: function () {
                const model = this.getOwnerComponent().getModel()
                sap.ui.require(["zhr237/controller/MessageParser"], function (MessageParser) {
                    const messageParser = new MessageParser(model.sServiceUrl, model.oMetadata, !!model.bPersistTechnicalMessages)
                    model.setMessageParser(messageParser)
                })
            },

            onEditDeskPosition: function (oEvent) {
                sap.ui.core.BusyIndicator.show(0)
                if (!this._schemaLayer)
                    this._schemaLayer = new SchemaLayer(this)
                sap.ui.core.BusyIndicator.hide()

                this._schemaLayer.display(oEvent.getSource().getProperty('target'))
            },

            handleFormClick: function (oEvent, typeName, mode) {
                const is_new = mode === 'NEW'

                let form
                switch (typeName) {
                    case "Layer": form = new FormLayer(this); break;
                    // case "Room": form = new FormRoom(this); break;
                    // case "Place": form = new FormPlace(this); break;
                }
                const keyField = `${typeName.toLowerCase()}_id`

                form.popup(
                    is_new,
                    is_new ? '' : oEvent.getSource().getTarget(),

                    function (update_item) {
                        const methodName = is_new ? "create" : "update"
                        const url = `/ZC_HR237_${typeName}${is_new ? '' : `('${update_item[keyField]}')`}`

                        this.getView().getModel()[methodName](url,
                            update_item,
                            {
                                success: function () {
                                    //const itemName = typeName === 'Place' ? 'Desk' : typeName
                                    Libs.showMessage.show(`${typeName} ${update_item[keyField]} was successfully ${methodName}ed`)
                                }.bind(this)
                            })

                    }.bind(this))
            },

            onBookClick: function (oEvent) {
                if (!this._NewBook)
                    this._NewBook = new NewBook(this)

                // By default booking for tomorrow
                const datum = Libs.get_noon(new Date())
                datum.setDate(datum.getDate() + 1)

                this._NewBook.showDialog(this.userInfo, {
                    datum: datum,
                    pernr: this.userInfo.pernr,
                    layer_id: '-',
                    persa: '-',
                    layer_text: 'Select Schema',
                    pickModeCallback: oEvent.pickModeCallback
                })
            },

            handleAppointmentSelect: function (oEvent) {
                const oAppointment = oEvent.getParameter("appointment")
                if (!oAppointment)
                    return

                sap.ui.require(["zhr237/controller/Details"], function (Details) {
                    if (!this.details)
                        this.details = new Details(this)
                    this.details.showPopup(oAppointment)
                }.bind(this))
            },

            handleSupportEmail: function () {
                sap.m.URLHelper.triggerEmail(
                    this.userInfo.support_email,
                    this.userInfo.support_subject,
                    this.userInfo.support_body)
            },

            showQrCode: function () {
                window.open(Libs.get_base_url() + `ZC_HR237_A_Show_Ticket(datum=datetime'${Libs.getDateIso(this.userInfo.nearest_book_date)}T00:00:00',pernr='${this.userInfo.pernr}')/$value`)
            },

        });
    });
