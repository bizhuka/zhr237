sap.ui.define([
    "zhr237/controller/Libs",

    "sap/ui/base/Object",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
], function (Libs, BaseObject, JSONModel, Filter, FilterOperator) {
    "use strict";

    return BaseObject.extend("zhr237.controller.CalendarBookingOverview", {
        _calendarData: {
            persons: [],
            specialDates: []
        },

        // No view. Use owners tab
        constructor: function (owner) {
            this.owner = owner

            this.calendarModel = new JSONModel()
            this.calendarModel.setData(this._calendarData)

            this.calendar = this.owner.byId('bookingCalendar')
            this.calendar.setModel(this.calendarModel, "calendar")
        },

        refresh_calendar: function () {

            this.owner.getOwnerComponent().getModel().read("/ZC_HR237_Booking", {
                filters: this._getFilters(),

                success: function (data) {
                    this._calendarData.specialDates = []
                    const persons = {}

                    for (const item of data.results) {
                        if (item.pernr === '88888888') {
                            this._calendarData.specialDates.push({
                                datum: item.datum,
                                tooltip: `${Libs.date_to_text(item.datum)} - ${item.ename}`
                            })
                            continue
                        }

                        const person = persons[item.pernr] ? persons[item.pernr] : {
                            title: `${item.pernr.replace(/^0+/, '')} - ${item.ename}`,
                            text: `${item.persa_txt} - ${item.department_txt}`,
                            avatar: Libs.get_avatar_url(item.pernr, 64),
                            bookings: [],
                            pernr: item.pernr
                        }
                        // Is empty?
                        if (item.datum.getFullYear() !== 9999) {
                            const day = this.geWholeDay(item.datum)
                            person.bookings.push({
                                startDate: day.low,
                                endDate: day.high,
                                title: item.layer_id,
                                text: item.place_text,
                                color: item.is_notified ? '#A1FB8E' : '#9CFFF5',
                                _src: item
                            })
                        }
                        persons[item.pernr] = person
                    }

                    this._calendarData.persons = Object.values(persons)
                    this.calendarModel.updateBindings()


                }.bind(this)
            })
        },

        _getFilters: function () {
            const days_count = this.calendar.getViewKey() === 'Week' ? 7 : 31

            const persaFilterKey = this.owner.getView().byId('id_pers_area_filter').getSelectedKey()
            return [
                new Filter("pernr", FilterOperator.EQ, '77777777'), // get own & direct subordinates data
                new Filter("datum", FilterOperator.EQ, Libs.getDateIso(Libs.get_noon(this.calendar.getStartDate()))),
                // TODO Fuzyy search ?
                new Filter("filter_days_count", FilterOperator.EQ, days_count),
                //new Filter("filter_show_dir_subo", FilterOperator.EQ, this.owner.userInfo.is_manager), //this.owner.byId('id_show_dir_subo').getState()
                new Filter("persa", FilterOperator.EQ,
                    persaFilterKey ? persaFilterKey :
                        this.owner.pickedDesk ? this.owner.pickedDesk.persa : ''),
            ]
        },

        showScheduleReport: function () {
            const objFilter = this._getFilters().reduce((acc, curr) => (acc[curr.sPath] = curr.oValue1, acc), {})
            window.open(Libs.get_base_url() + `ZC_HR237_A_Schedule_Report(begda=datetime'${objFilter.datum}T00:00:00',persa='${objFilter.persa}')/$value`)
        },

        geWholeDay: function (date) {
            const low = new Date(date.getTime() + date.getTimezoneOffset() * 60000)

            const high = new Date(low.getTime());
            high.setDate(high.getDate() + 1)

            return {
                low: low,
                high: high
            }
        },

    });
}
);