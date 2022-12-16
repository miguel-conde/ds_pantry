# rm(list=ls());gc()

library(XLConnect)
library(googleVis)


nombre <- "Sankey_Leads"

wb <- loadWorkbook("plotting/SankeyLeads2.xlsx")
data <- readWorksheet(wb,sheet=1)
summary(data)
# % absoluto
data$Leads <- abs(data$Leads)/
  sum(data$Leads[data$Target=="Leads"])*100


data$Weight <- sqrt(data$Leads)
data <- data[,c("Explanatory","Target","Weight","Leads")]


plot(
  gvisSankey(
              data, 
              from="Explanatory", 
              to="Target", 
              weight="Weight",
              options=list(
                            forcelFrame=FALSE,
                            height=700,
                            width=1600,
                            sankey="{
                                      iterations: 100000, //Default:32. N?mero de pruebas que hace para encontrar la configuraci?n que mejor se pueda leer. Juega con la posici?n de los nodos. De esto se encarga 'D3 layout engine'. 
                                      link: {
                                              color: { 
                                                       fill: '#EEB61A',//'#58ACFA', //'#b00003',  Color of the link.
                                                       //fill0pacity: 0.8, // Transparency of the link.
                                                       //stroke: 'white', // Color of the link border. Default: sin borde
                                                       //strokeWidt: 0 // Thickness of the link border (default 0).
                                                     },
                                              colors: [
                                                        '#a6cee3',
                                                        '#1f78b4',
                                                        '#b2df8a',
                                                        '#FE9A2E',
                                                        '#F781D8',
                                                        '#FA5858',
                                                        '#33a02c'
                                                      ],
                                              colorMode:
                                                        //'source'  //el link coge el color del nodo origen
                                                        //'target'  //el link coge el color del nodo destino
                                                        //'gradient'  //el link coge un color gradual entre el nodo origen y el nodo destino
                                                        'none' //default. Coge el sankey.link.color.fill 
                                            },
                                      node: { 
                                              width: 6, 
                                              //color: { fill: '#b00003' },
                                              colors: [
                                                        '#363BDC',         // Custom color palette for sankey nodes.
                                                        '#b00003',         // Nodes will cycle through this palette
                                                        '#E9550B',         // giving each node its own color.
                                                        '#33a02c',
                                                        '#1e1011',
                                                        '#00EBFF',
                                                        '#F781D8',
                                                        '#800080'
                                                      ],
                                              label: { 
                                                      fontName: 'Trebuchet MS',
                                                      fontSize: 14,
                                                      color: '#871b47',
                                                      bold: true
                                                     }
                                              ,colorMode: 'unique'
                                              
                                            }
                                   }"
                          ),
             chartid=nombre
           )
)



