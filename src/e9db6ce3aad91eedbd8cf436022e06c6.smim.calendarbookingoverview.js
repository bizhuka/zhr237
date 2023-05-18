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
            persons: []
        },

        // No view. Use owners tab
        constructor: function (owner) {
            this.owner = owner
            this.libs = new Libs()

            this.calendarModel = new JSONModel()
            this.calendarModel.setData(this._calendarData)

            const calendar = this.owner.byId('bookingCalendar')
            calendar.setModel(this.calendarModel, "calendar")
        },

        refresh_calendar: function () {
            const calendar = this.owner.byId('bookingCalendar')
            const days_count = calendar.getViewKey() === 'Week' ? 7 : 31

            this.owner.getOwnerComponent().getModel().read("/ZC_HR237_Booking", {
                filters: [
                    new Filter("pernr", FilterOperator.EQ, '77777777'), // get own & direct subordinates data
                    new Filter("datum", FilterOperator.EQ, this.libs.getDateIso(this.libs.get_noon(calendar.getStartDate()))),
                    // TODO real filter by layer_id & pernr ?  Fuzyy search ?
                    new Filter("layer_id", FilterOperator.EQ, days_count),
                ],

                success: function (data) {
                    const persons = {}
                    for (const item of data.results) {
                        const person = persons[item.pernr] ? persons[item.pernr] : {
                            title: item.pernr,
                            text: item.ename,
                            avatar: this.libs.get_avatar_url(item.pernr, 64),
                            bookings: []
                        }
                        const day = this.geWholeDay(item.datum)
                        person.bookings.push({
                            startDate: day.low,
                            endDate: day.high,
                            title: item.layer_id,
                            text: item.place_text,
                            color: '#9CFFF5',
                            _src: item
                        })
                        persons[item.pernr] = person
                    }

                    this._calendarData.persons = Object.values(persons)
                    this.calendarModel.updateBindings()


                }.bind(this)
            })
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