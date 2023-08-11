sap.ui.define([
	"zhr237/controller/Libs",

	"sap/ui/base/Object",
	"sap/ui/model/json/JSONModel",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	// "sap/viz/ui5/controls/Popover",
	"sap/viz/ui5/format/ChartFormatter",
	"sap/viz/ui5/controls/common/feeds/FeedItem",
	"sap/viz/ui5/data/MeasureDefinition"
], function (Libs, SapObject, JSONModel, Filter, FilterOperator, ChartFormatter, FeedItem, MeasureDefinition) {
	"use strict";

	return SapObject.extend("zhr237.controller.ChartTab", {
		owner: null,

		_chart: {
			_dynamics: [],
			_depart_items: [],

			_period_key: 'WEEK',
			_orgeh_filter: '00000000',
			_periods: [
				{ key: 'DAY', value: 'Day' },
				{ key: 'WEEK', value: 'Week' },
				{ key: 'MONTH', value: 'Month' },
				{ key: 'QUARTER', value: 'Quarter' },
				{ key: 'YEAR', value: 'Year' },
			]
		},

		constructor: function (owner) {
			this.owner = owner

			sap.ui.core.BusyIndicator.show(0)
			sap.ui.core.Fragment.load({ id: owner.getView().getId(), name: "zhr237.fragment.ChartTab", controller: this })
				.then(function (chartTab) {
					this._bindPopOver('frameDepart', 'popOverDynamics')
					this._bindPopOver('frameDynamics', 'popOverDepart')

					this.chartModel = new JSONModel()
					this.chartModel.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
					this.chartModel.setData(this._chart)

					chartTab.setModel(this.chartModel, "chart")
					this._read_all_chart()

					// ChartContainerSelectionDetails._initializeSelectionDetails(oContent)
					// oChartContainer.updateChartContainer()

					owner.getView().byId('chartVBox').addItem(chartTab)
					sap.ui.core.BusyIndicator.hide()
				}.bind(this))

		},

		_bindPopOver: function (idFrame, idPopOver) {
			const oPopOver = this.owner.getView().byId(idPopOver)
			oPopOver.connect(this.owner.getView().byId(idFrame).getVizUid())
			// oPopOver.setFormatString(ChartFormatter.DefaultPattern.SHORTFLOAT) // STANDARDFLOAT
		},

		_setMaxCapacityLine: function (id, items) {
			const frame = this.owner.getView().byId(id)
			const vizProperties = frame.getVizProperties()
			const valueAxis = vizProperties.plotArea.referenceLine.line.valueAxis

			const isVisible = items.length > 0

			// Max. capacity
			valueAxis[0].visible = isVisible
			valueAxis[0].value = isVisible ? Number(items[0].target_cnt) : 0

			// Average value
			valueAxis[1].visible = isVisible
			valueAxis[1].value = isVisible ? items.reduce((total, next) => total + Number(next.cnt), 0) / items.length : 0
			valueAxis[1].label.text = `Average value ${Math.round(valueAxis[1].value * 100) / 100}`

			if (isVisible)
				vizProperties.plotArea.primaryScale.maxValue = Math.round(valueAxis[0].value * 1.04)
			frame.setVizProperties(vizProperties)
		},

		_fillDynamics: function (items) {
			this._chart._dynamics = []

			for (let item of items)
				this._chart._dynamics.push({
					period: item.period,
					period_raw: item.period_raw,
					cnt: Number(item.cnt),
					target_cnt: Number(item.target_cnt)
				})
			this._chart._dynamics.sort((a, b) => a.period_raw > b.period_raw ? 1 : -1)

			this._setMaxCapacityLine('frameDynamics', items)
		},

		_fillSplitByDeprtment: function (items) {
			const unqDepart = {}
			const unqItems = {}
			for (const item of items) {
				const updateLine = unqItems[item.period] ? unqItems[item.period] : {
					period: item.period,
					period_raw: item.period_raw
				}
				unqItems[item.period] = updateLine

				const name = `d${item.department}`
				updateLine[name] = (updateLine[name] ? updateLine[name] : 0) + Number(item.cnt)
				unqDepart[name] = item.department_txt
			}
			this._chart._depart_items = Object.values(unqItems)
			this._chart._depart_items.sort((a, b) => a.period_raw > b.period_raw ? 1 : -1)

			// Delete previous feeds
			const frameDepart = this.owner.getView().byId('frameDepart')
			const feeds = frameDepart.getFeeds()
			for (let i = 1; i < feeds.length; i++) //0 is Dimension period
				frameDepart.removeFeed(feeds[i])

			const datasetDepart = this.owner.getView().byId('datasetDepart')
			datasetDepart.removeAllMeasures()

			const arr_department_texts = []
			for (const department_key in unqDepart) {
				const department_text = unqDepart[department_key]
				arr_department_texts.push(department_text)

				datasetDepart.addMeasure(new MeasureDefinition({
					value: `{chart>${department_key}}`,
					name: department_text
				}))
			}

			frameDepart.addFeed(new FeedItem({
				uid: "valueAxis",
				type: "Measure",
				values: arr_department_texts
			}))
		},

		_read_all_chart: function () {
			this.owner.getOwnerComponent().getModel().read("/ZC_HR237_Chart", {
				filters: [
					new Filter("period", FilterOperator.EQ, this._chart._period_key),
					new Filter("department", FilterOperator.EQ, this._chart._orgeh_filter)],

				urlParameters: {
					"$select": "cnt,period,department,department_txt,chart_kind,target_cnt,period_raw"
				},

				success: function (data) {
					this._fillDynamics(data.results.filter(item => item.chart_kind === 'dynamics_0'))
					this._fillSplitByDeprtment(data.results.filter(item => item.chart_kind === 'deprtment_1'))
					this.chartModel.updateBindings()

				}.bind(this)
			})

		}

	});
}
);