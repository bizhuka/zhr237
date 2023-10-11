sap.ui.define([
    "zhr237/controller/Libs",
    "zhr237/controller/DeskEdit",

    "sap/ui/base/Object",
    "sap/suite/ui/commons/networkgraph/layout/NoopLayout",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
], function (Libs, DeskEdit, Object, NoopLayout, Filter, FilterOperator) {
    "use strict";

    return Object.extend("zhr237.controller.SchemaLayer", {
        owner: null,
        _item: {
            title: "",
            editMode: false,
            nodes: [],
            rooms: []
        },

        constructor: function (owner) {
            this.owner = owner

            this._model = new sap.ui.model.json.JSONModel()
            this._model.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            this._model.setData(this._item)
        },

        display: function (target, ext) {
            this.ext = ext ? ext : {}

            const params = target.split('^')
            const layer_id = params[0]
            const layer_text = params[1]

            this.layer_id = layer_id
            this._item.editMode = !this.ext.addFilter
            this._item.chooseText = "Choose desk"
            this._item.chooseEnabled = false

            this._item.title = `${this._item.editMode ? 'Edit desks' : 'Choose desk'} of ${layer_text} (${layer_id})`

            if (!this._dialog) {
                this._dialog = sap.ui.xmlfragment("zhr237.fragment.SchemaLayer", this)
                this._dialog.setModel(this._model, "item")
                this.owner.getView().addDependent(this._dialog)

                this._graph = sap.ui.getCore().byId("graph")
                this._graph.setLayoutAlgorithm(new NoopLayout())
            }

            // sap.ui.core.BusyIndicator.show(0)  sap.ui.core.BusyIndicator.hide()
            // sap.ui.core.Fragment.load({
            //     name: "zhr237.fragment.SchemaLayer", controller: this
            // }).then(function (result) { return result    
            // })
            this.fillAndShow(layer_id)
        },

        fillAndShow: function (layer_id) {
            for (let node of this._graph.getNodes().filter(item => item.getSelected()))
                node.setSelected(false)
            this._item.nodes = []
            this._item.rooms = []

            let arrFilter = [
                new Filter("layer_id", FilterOperator.EQ, layer_id),
                new Filter("place_x", FilterOperator.NE, 0),
                new Filter("place_y", FilterOperator.NE, 0)]

            if (this.ext && this.ext.addFilter)
                arrFilter = [...arrFilter, ...this.ext.addFilter]

            this.owner.getOwnerComponent().getModel().read("/ZC_HR237_Place", {
                filters: arrFilter,
                urlParameters: {
                    "$select": `place_id,place_text,place_x,place_y,orgtx`
                },

                success: function (data) {
                    if (this.ext && this.ext.skip_desks)
                        for (let desk of this.ext.skip_desks) {
                            let elem = data.results.find(element => element.place_id === desk.place_id)
                            if (elem) {
                                elem.pernr = desk.pernr
                                elem.ename = desk.ename
                                elem.user_name = desk.user_name
                                elem.created_when = desk.created_when
                                //elem.orgtx = `${elem.orgtx} - booked`
                            }
                        }

                    for (let place of data.results)
                        this._add1Desk(place)

                    if (data.results.length === 0)
                        this._add1Desk({
                            place_id: 999,
                            place_text: '',
                            place_x: 1,
                            place_y: 1,
                            orgtx: ''
                        })
                    this._model.updateBindings()

                    // Direct url do not work
                    const url_with_$ = `${window.location.origin}/sap/opu/odata/sap/ZC_HR237_BOOKING_CDS/ZC_HR237_Layer('${layer_id}')/`
                        + encodeURIComponent("$value") // + '?ok.png'
                    this.toDataURL(url_with_$, function (result) {
                        this._graph.setBackgroundImage(result)
                    }.bind(this))

                    this._dialog.open()
                }.bind(this)
            })
        },


        toDataURL: function (url, callback) {
            var xhr = new XMLHttpRequest();
            xhr.onload = function () {
                var reader = new FileReader()
                reader.onloadend = function () {
                    callback(reader.result)
                }
                reader.readAsDataURL(xhr.response)
            };
            xhr.open('GET', url)
            xhr.responseType = 'blob'
            xhr.send()
        },

        _add1Desk: function (item, tryUpdate) {
            for (let i = 0; i === 0; i++) {
                if (!tryUpdate) break

                const previous = this._item.nodes.filter(line => line.key === item.place_id)[0]
                if (!previous) break

                if (item.place_x && item.place_y) {
                    previous.x = item.place_x
                    previous.y = item.place_y
                }
                previous.title = `${item.place_text}` //- ${item.orgtx}`
                return
            }

            this._item.nodes.push({
                key: item.place_id,
                title: `${item.place_text}`, //- ${item.orgtx}`,
                x: item.place_x,
                y: item.place_y,
                status: item.ename ? "Error" : "Success",
                attributes: item.ename ? [
                    { label: item.pernr, value: item.ename },
                    {
                        label: item.created_when.toLocaleDateString('ru-RU', {
                            day: "2-digit",
                            month: "2-digit",
                            year: "numeric",
                            hour: "2-digit",
                            minute: "2-digit",
                            hour12: false
                        }), value: item.user_name
                    }] : null
            })
        },

        _onAfterDialogOpen: function () {
            const toolbar = sap.ui.getCore().byId("graph-toolbar")
            for (let button of toolbar.getContent())
                try {
                    switch (button.getTooltip()) {
                        case 'Zoom to Fit':
                            button.firePress()
                            break;
                        case 'Enter Full Screen':
                            button.setVisible(false)
                            break;
                    }
                } catch (e) {
                    console.error(e)
                }
            document.getElementById("graph-innerscroller").addEventListener("click", this._onClick.bind(this))
        },

        handleEditDesk: function () {
            if (!this._DeskEdit)
                this._DeskEdit = new DeskEdit(this.owner)

            this.current_desk = null
            this._DeskEdit.showDialog(this.layer_id, function (action, is_new, current_desk) {
                this.current_desk_new = is_new
                this.current_desk = current_desk
                if (action === 'SAVE')
                    this._doSave()
            }.bind(this))
        },

        _doSave: function () {
            const methodName = this.current_desk_new ? "create" : "update"
            const deskUrl = "/ZC_HR237_Place" + (this.current_desk_new ? "" : `('${this.current_desk.place_id}')`)

            const fullCopy = this.current_desk
            this.current_desk = null

            const updateItem = { ...fullCopy }
            delete updateItem.place_text
            delete updateItem.orgtx

            const _this = this
            this.owner.getView().getModel()[methodName](deskUrl,
                updateItem,
                {
                    success: function (current_desk) {
                        current_desk = current_desk ? current_desk : fullCopy
                        Libs.showMessage(`Desk ${current_desk.place_id} was successfully ${methodName}ed`)
                        _this._add1Desk(current_desk, true)
                        _this._model.updateBindings()
                    }
                })
        },

        _onClick: function (evt) {
            if (!this.current_desk)
                return
            const dim = evt.target.getBoundingClientRect()
            const zoom = this._graph.getCurrentZoomLevel()
            this.current_desk.place_x = Math.round((evt.clientX - dim.left) / zoom) - 50
            this.current_desk.place_y = Math.round((evt.clientY - dim.top) / zoom)

            this._doSave()
        },

        handleEditCancelButton: function () {
            this._dialog.close()
        },

        onSelectionChange: function () {
            const selectedArr = this._graph.getNodes()
                .filter(item => item.getSelected())
            const node = selectedArr.length === 1 && selectedArr[0].getStatus() === 'Success' ? selectedArr[0] : null

            this._item.chooseText = node ? `Choose desk '${node.getKey().substr(3)}'` : "Choose desk"
            this._item.chooseEnabled = !!node
            this.ext.selected_place_id = node ? node.getKey() : null
            this._model.updateBindings()
        },

        handleDeskChoosed: function () {
            if (!this.ext.selected_place_id) return

            this.ext.callBack(this.ext.selected_place_id)
            this._dialog.close()
        },

        _doAlign: function () {
            const selectedArr = this._graph.getNodes()
                .filter(item => item.getSelected())
                .map(item => {
                    return {
                        place_id: item.getKey(),
                        place_x: item.getX(),
                        place_y: item.getY()
                    }
                })
            if (selectedArr.length < 2) {
                Libs.showMessage('Select more items', true)
                return
            }

            const calcX = this.getStandardDeviation(selectedArr, "place_x")
            const calcY = this.getStandardDeviation(selectedArr, "place_y")

            if (calcX.deviation >= 7 && calcY.deviation >= 7) {
                Libs.showMessage('Deviation is too big', true)
                return
            }
            const calc = calcX.deviation > calcY.deviation ? calcY : calcX            

            for (let item of selectedArr) {
                item[calc.field] = calc.mean
                this.owner.getView().getModel().update(`/ZC_HR237_Place('${item.place_id}')`, item)
            }
            Libs.showMessage(`${selectedArr.length} items aligned`)
            this._graph.deselect()
        },

        getStandardDeviation: function (array, field) {
            const n = array.length
            const mean = array.reduce((a, b) => a + b[field], 0) / n
            return {
                deviation: Math.sqrt(array.map(x => Math.pow(x[field] - mean, 2)).reduce((a, b) => a + b) / n),
                mean: Math.round(mean),
                field: field
            }
        }

    });
}
);