#####0. Packages and libraries ####

  library(sqldf) #SQL querys
  library(mgsub) #Dealing with text data
  library(stringr)
  library(readxl)
  library(leaflet)
  library(leaflet.minicharts)
  library(htmlwidgets)
  library(RColorBrewer)
  library(xml2)
  library(dplyr)
  library(randomcoloR)
  library(kulife)
  library(XML)
 
#  setwd("~/HEC/Terms/SecondTerm/DistributionManagement/Practical assigment/Code app")

####1. Choices####
  
  getChoices <- function(){
    choices <- list.files(path="./instances/",pattern=".xml")
    choices <- gsub(".xml","",choices)
    return(choices)
  }
  
  
#### Define UI for application that draws a histogram####

ui <- fixedPage(
  
  # Application title
  
  fixedRow(column(8,offset=4,tags$h1("The electric vehicle routing problem with non-linear charging function"))),
 
  #Create the two panels
  
  tabsetPanel(
      tabPanel("Introduction",
               HTML("<br><br>"),
               HTML(paste0(
                 "<p><a>The electric vehicle routing problem with non-linear charging function</a> was first introduced by introduced by Montoya et al. (2017).
                 The problem can be formally defined as follows. 
                 Let I be a set set of customers that need to be served and 
                 F be the set of charging stations (CSs) 
                 at which vehicles may stop to recharge their battery. 
                 Each customer i has a service time g_i. The customers are served 
                 using a homogeneous fleet of EVs. 
                 Each EV has a battery of capacity Q (expressed in kWh).
                 At the beginning of the planning horizon, the EVs are located in a single depot, 
                 from which they leave fully charged. 
                 The depot is continuously open for Tmax hours. Traveling from one location i 
                 (the depot, a customer, or a CS) to another 
                 location j incurs a driving time t_ij greater or equal to zero and an energy 
                 consumption eij greater or equal to zero. 
                 Driving times and energy consumption both satisfy the triangle inequality. 
                 Due to their limited battery capacity, EVs may have to stop en route at CSs. 
                 Charging operations can occur at any CS, they are non-preemptive, and EVs 
                 can be partially recharged."
                 )),
               HTML("<br><br>"),
               HTML(paste0(
                 "<p>In this app, you will find the solutions for each of the instances proposed by Montoya et al. (2017).
                 Instances are named using the following convention: tcAcBsCcDE, where:
                  - A is the method used to place the customers (i.e., 0: random uniform, 1: clustered, 2: mixture of both)
                  - B is the number of customers
                  - C is the number of the CSs, 
                  - D is 't' if the CSs are located using a p-median heuristic and 'f' if the CSs were randomly located
                  - E is the number of the instance for each combination of parameters (i.e., E={0,1,2,3,4})."
                )),
               HTML("<br><br>"),
               HTML(paste0(
                 "<p>Finally, the user can use the final panel to test their own solutions for each instance.
                    A solution must be submitted using a xml file. In the following link you can find an example of the format: <a>http://dimacs.rutgers.edu/files/5116/4158/6077/tc2c10s2cf0.xml</a>
                    A solution checker is also available at: <a>https://github.com/ncabrera10/SolutionChecker_E-VRP</a>.
                 </p>"
                ))
              ),
      tabPanel("Solutions",
               
               HTML("<br><br>"),
               
               # Select the county, the instance number and the algorithm
               
               sidebarPanel(
                 selectInput("instance", label = "Select the instance",
                             choices = getChoices(),selected = "tc0c10s2cf1"),
                 selectInput("algorithm", label = "Select the algorithm",
                             choices = c("ILS","BKS"),selected = "ILS")
               ),
               
               # Show the table
               
               mainPanel(tableOutput("table1")),
               
               fluidRow(),
               column(8,offset = 4,plotOutput("graph",width = "800px",height="800px"))
               
    ),tabPanel("New solutions tester",
               
               HTML("<br><br>"),
               sidebarPanel(
                 selectInput("instanceT", label = "Select the instance",
                             choices = getChoices(),selected = "tc0c10s2cf1"),
                 fileInput('file1', 'Select the XXX.xml file',
                           accept=c('text/txt','text/comma-separated-values,text/plain','.txt')),
                 downloadButton("downloadData", label = "Solution example for this instance")
               ),
          
               mainPanel(
                 tableOutput("tableT"),
                 plotOutput("graphT",width = "800px",height="800px")
               )
               
      )
    )
)
  
  

# Define server logic required 

  server <- function(input, output) {
    
    # Route's information
    
    # Create a reactive table 
    
    routesTable <- reactive({createTable(input$instance,input$algorithm)})
    
    # Create the output object
    
    output$table1 <- renderTable({routesTable()},striped = TRUE,align = 'c')
  
    # Creates a reactive graph
    
    routeGraph <- reactive({createGraph(input$instance,input$algorithm)})
    output$graph <- renderPlot({routeGraph()})
    
    datasetInput <- reactive({findSolution(input$instanceT)})
    datasetName <- reactive({findName(input$instanceT)})
    
    output$downloadData <- downloadHandler(
      filename = function() {
        datasetName()
      },
      content = function(file) {
        #print(datasetInput())
        write_xml(datasetInput(),file)
        #saveXML(datasetInput(),file)
        #write.xml(datasetInput(),file)
        #write.table(datasetInput(), file,sep=";",row.names=FALSE,quote = FALSE,col.names = FALSE)
      }
    )
    
  
    
    observe({
      file1 = input$file1
      if (is.null(file1)){
        return(NULL)
      }
      #data1 = read.csv(file1$datapath, header=FALSE, sep=";")
      data1 = read_xml(file1$datapath) %>% as_list()
      routeGraphT <- reactive({createGraphT(input$instanceT,data1)})
      routesTableT <- reactive({createTableT(input$instanceT,data1)})
      output$tableT <- renderTable({routesTableT()},striped=TRUE,align='c')
      output$graphT <- renderPlot({routeGraphT()})
    })
    
  }

####4. Functions####
 
  # Function to read an instance:
  
  loadInstance <- function(insNum){
    
    # Read the xml file:
    
    listOfElements <-  read_xml(paste0("./instances/",insNum,".xml")) %>% as_list()
    
    # General information:
    
    numCustomers = 0
    depotID = -1
    vecNodes = c()
    vecTypes = c()
    vecCoorsX = c()
    vecCoorsY = c()
    vecTypeCS = c()
    
    # Read the coordinates:
    
    for(i in listOfElements$instance$network$nodes){
      node_id = as.integer(attributes(i)[["id"]])
      node_type = as.integer(attributes(i)[["type"]])
      node_cx = as.double(i$cx)
      node_cy = as.double(i$cy)
      if(node_type == 0){
        depotID = node_id
      }
      if(node_type == 2){
        vecTypeCS = c(vecTypeCS,i$custom$cs_type[[1]])
      }else{
        vecTypeCS = c(vecTypeCS,"NotACS")
      }
      vecNodes = c(vecNodes,node_id)
      vecTypes = c(vecTypes,node_type)
      vecCoorsX = c(vecCoorsX,node_cx)
      vecCoorsY = c(vecCoorsY,node_cy)
      
    }
    
    nodes_df = data.frame(vecNodes,vecTypes,vecCoorsX,vecCoorsY,vecTypeCS)
    colnames(nodes_df) = c("ID","Type","CoorX","CoorY","TypeOfCS")
    
    # Read the fleet and the charging functions info:
    
    type_of_functions = c()
    battery_points = list()
    charging_times = list()
    for(i in listOfElements$instance$fleet){

      speed_factor = as.double(i$speed_factor)
      max_travel_time = as.double(i$max_travel_time)
      consumption_rate = as.double(i$custom$consumption_rate)
      battery_capacity = as.double(i$custom$battery_capacity)
      
      for(j in i$custom$charging_functions){
        
        break_actual = c()
        times_actual = c()
        
        type_of_functions = c(type_of_functions,(attributes(i)[["cs_type"]]))
        
        iter = 1
        dataAux = as.data.frame(unlist(j))
        for(k in 1:nrow(dataAux)){
          
          if(iter == 1){
            break_actual = c(break_actual,as.double(dataAux[k,1]))
            
            battery_points[[(attributes(j)[["cs_type"]])]] = break_actual
            iter = 0
          }
          else {
            times_actual = c(times_actual,as.double(as.double(dataAux[k,1])))
            
            charging_times[[(attributes(j)[["cs_type"]])]] = times_actual
            
            iter = 1
          }
          
        }
        
       
      }
    }
    
    
    #Service times:
    
    nodos_c = c()
    service_times = c()
    for(i in listOfElements$instance$requests){
      
      nodos_c = c(nodos_c,as.integer(attributes(i)[["id"]]))
      service_times = c(service_times,as.double(i$service_time))
    }
    
    service_times_df = data.frame(nodos_c,service_times)
    colnames(service_times_df) = c("ID","ServiceTime")
    
    listOfElements = list()
    listOfElements[["service_times_df"]] = service_times_df
    listOfElements[["nodes_df"]] = nodes_df
    listOfElements[["battery_points"]] = battery_points
    listOfElements[["charging_times"]] = charging_times
    listOfElements[["consumption_rate"]] = consumption_rate
    listOfElements[["speed_factor"]] = speed_factor
    listOfElements[["battery_capacity"]] = battery_capacity
    listOfElements[["max_travel_time"]] = max_travel_time
    
    # Time and energy matrix:
    
    tail = c()
    head = c()
    time = c()
    energy = c()
    for(i in 1:nrow(nodes_df)){
      
      for(j in 1:nrow(nodes_df)){
        
        tail = c(tail,nodes_df[i,1])
        head = c(head,nodes_df[j,1])
        dist = sqrt((nodes_df[i,3] - nodes_df[j,3])^2+(nodes_df[i,4] - nodes_df[j,4])^2)
        time = c(time,dist/speed_factor)
        energy = c(energy,dist * consumption_rate)
      }
      
      resources_matrix = data.frame(tail,head,time,energy,key=paste0(tail,";",head))
      
    }
    
    listOfElements[["resources_matrix_df"]] = resources_matrix
    
    # Return the list of elements:
    
    return(listOfElements)
  }
  
  # Function to read an instance:
  
  loadInstance_soft <- function(insNum){
    
    # Read the xml file:
    
    listOfElements <-  read_xml(paste0("./instances/",insNum,".xml")) %>% as_list()
    
    # General information:
    
    numCustomers = 0
    depotID = -1
    vecNodes = c()
    vecTypes = c()
    vecCoorsX = c()
    vecCoorsY = c()
    vecTypeCS = c()
    
    # Read the coordinates:
    
    for(i in listOfElements$instance$network$nodes){
      node_id = as.integer(attributes(i)[["id"]])
      node_type = as.integer(attributes(i)[["type"]])
      node_cx = as.double(i$cx)
      node_cy = as.double(i$cy)
      if(node_type == 0){
        depotID = node_id
      }
      if(node_type == 2){
        vecTypeCS = c(vecTypeCS,i$custom$cs_type[[1]])
      }else{
        vecTypeCS = c(vecTypeCS,"NotACS")
      }
      vecNodes = c(vecNodes,node_id)
      vecTypes = c(vecTypes,node_type)
      vecCoorsX = c(vecCoorsX,node_cx)
      vecCoorsY = c(vecCoorsY,node_cy)
      
    }
    
    nodes_df = data.frame(vecNodes,vecTypes,vecCoorsX,vecCoorsY,vecTypeCS)
    colnames(nodes_df) = c("ID","Type","CoorX","CoorY","TypeOfCS")
    
    
    listOfElements = list()
    listOfElements[["nodes_df"]] = nodes_df
   
    # Return the list of elements:
    
    return(listOfElements)
  }
  
  # A function to calculate the charging time:
  
  calculateChargingTime <- function(charging_time_vec,battery_points_vec,currentSoC,energy_charged){
 
    
    #charging_time_vec = info_instance$charging_times[[tableOrder_aux$cs_type[i]]]
    #battery_points_vec = info_instance$battery_points[[tableOrder_aux$cs_type[i]]]
    #currentSoC = energy_vec[length(energy_vec)]
    #energy_charged = tableOrder_aux$charge_time[i]
    initialSoC = currentSoC
    finish = FALSE
    current_break = 1
    energy_already_charged = 0
    time_charging = 0
    iter = 0
    while(finish == FALSE && iter < 3){
    
      iter = iter + 1
      current_m = (battery_points_vec[current_break+1] - battery_points_vec[current_break])/(charging_time_vec[current_break+1] - charging_time_vec[current_break])
      current_b = battery_points_vec[current_break+1] - current_m * charging_time_vec[current_break+1] 
      if(currentSoC <= battery_points_vec[current_break+1]){
        
        charge_break = min(battery_points_vec[current_break+1],initialSoC + energy_charged)
        
        time_1 = (currentSoC - current_b)/current_m
        time_2 = (charge_break - current_b)/current_m
        
        time_charging = time_charging + time_2 - time_1
        energy_already_charged =energy_already_charged + min(battery_points_vec[current_break+1],initialSoC + energy_charged) - currentSoC
        currentSoC =  min(battery_points_vec[current_break+1],initialSoC + energy_charged)
        current_break = current_break + 1
        if(energy_already_charged == energy_charged){
          finish = TRUE
        }
      }
      
    }
    
    return(time_charging)
    
  }
  
  #loadInstance("tc1c10s3ct4")
  
  loadSolution <- function(insNum,algor,info_instance){
    
    if(algor == "ILS"){
      cadena = paste0("/sol",algor,"Best_") 
    }
    if(algor == "BKS"){
      cadena = paste0("/sol_EVRPNL_",algor,"_")
    }
    
    # Read the xml file:
    
    listOfElements <-  read_xml(paste0("./solutions/",algor,cadena,insNum,".xml")) %>% as_list()
    
    # Read each route:
    
    info_routes = list()
    
    for(route in listOfElements$solution){
      
      start_time = as.double(route$start_time)
      id_route = as.integer(attributes(route)[["id"]])
      
      # The sequence:
      
      nodes_id = c()
      charging_times = c()
      waiting_times = c()
      cs_types = c()
      currentSoC = info_instance$battery_capacity
      for(node in route$sequence){
        
        if(length(node) > 1){
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])
          type_of_cs = info_instance$nodes_df$TypeOfCS[info_instance$nodes_df$ID == attributes(node)[["id"]]]
          cs_types = c(cs_types,type_of_cs)
          charging_times = c(charging_times,as.double(node$charge))
          waiting_times = c(waiting_times,as.double(node$wait))
          
        }else{
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])
          charging_times = c(charging_times,0)
          waiting_times = c(waiting_times,0)
          cs_types = c(cs_types,"NotACS")
        }
      }
      
      head = nodes_id[2:length(nodes_id)]
      tail = nodes_id[1:(length(nodes_id)-1)]
      
      order_df = data.frame(tail,head,charge_time = charging_times[1:length(tail)],waiting_time = waiting_times[1:length(tail)],key=paste0(tail,";",head),cs_type = cs_types[1:length(tail)])
      list_info_this_route = list()
      list_info_this_route[["start_time"]] = start_time
      list_info_this_route[["order_df"]] = order_df
      info_routes[[paste0("",id_route)]] = list_info_this_route  
      
    }
    rm(listOfElements)
    return(info_routes)
    
    
  }
  
  loadSolution_soft <- function(insNum,algor,info_instance){
    
    if(algor == "ILS"){
      cadena = paste0("/sol",algor,"Best_") 
    }
    if(algor == "BKS"){
      cadena = paste0("/sol_EVRPNL_",algor,"_")
    }
    
    # Read the xml file:
    
    listOfElements <-  read_xml(paste0("./solutions/",algor,cadena,insNum,".xml")) %>% as_list()
    
    # Read each route:
    
    info_routes = list()
    
    for(route in listOfElements$solution){
      
      id_route = as.integer(attributes(route)[["id"]])
      
      # The sequence:
      
      nodes_id = c()
      for(node in route$sequence){
        
        if(length(node) > 1){
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])

        }else{
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])

        }
      }
      
      head = nodes_id[2:length(nodes_id)]
      tail = nodes_id[1:(length(nodes_id)-1)]
      
      order_df = data.frame(tail,head,key=paste0(tail,";",head))
      list_info_this_route = list()
      list_info_this_route[["order_df"]] = order_df
      info_routes[[paste0("",id_route)]] = list_info_this_route  
      
    }
    rm(listOfElements,info_instance)
    return(info_routes)
    
    
  }

  #Function to evaluate a solution:

  createTable <- function(insNum,algor){
    
    # Load the instance:
    
    info_instance = loadInstance(insNum)
    
    # Load the solution:
    
    info_solution = loadSolution(insNum,algor,info_instance)
    
    # Calculate the total time for each route:
    
    route_id = c()
    driving_time = c()
    charging_time = c()
    service_time = c()
    waiting_time = c()
    energy_consumption = c()
    start_time = c()
    iter = 1
    for(route in info_solution){
      tableResources = info_instance$resources_matrix_df
      tableOrder = route$order_df
      tableMix = tableResources[tableResources$key %in% unique(tableOrder$key),]
      
      nodos = unique(c(tableOrder$tail,tableOrder$head))
      
      tableOrder_aux = sqldf("SELECT a.*,b.energy,b.time from tableOrder as a LEFT JOIN 
                             (SELECT key,energy,time FROM tableResources) as b
                             on a.key = b.key")
      
      energy_vec = c()
      energy_vec = c(energy_vec,info_instance$battery_capacity)
      charging_times_f = c()
      for(i in 1:nrow(tableOrder_aux)){
      
        if(tableOrder_aux$cs_type[i] == "NotACS"){
          
          energy_vec = c(energy_vec,energy_vec[length(energy_vec)] - tableOrder_aux$energy[i])
          charging_times_f = c(charging_times_f,0)
          
        }else{
          
          charging_times_f = c(charging_times_f,calculateChargingTime(info_instance$charging_times[[tableOrder_aux$cs_type[i]]],info_instance$battery_points[[tableOrder_aux$cs_type[i]]],energy_vec[length(energy_vec)],tableOrder_aux$charge_time[i]))
          energy_vec = c(energy_vec,energy_vec[length(energy_vec)] - tableOrder_aux$energy[i] + tableOrder_aux$charge_time[i])
        }
        
      }
      
      tableOrder_aux$energy_at_each_time = energy_vec[2:length(energy_vec)]
      tableOrder_aux$charging_times = charging_times_f
      
      # Store them all: 
      
      start_time = c(start_time,route$start_time)
      energy_consumption = c(energy_consumption,sum(tableMix$energy))
      driving_time = c(driving_time,sum(tableMix$time))
      route_id = c(route_id,iter)
      service_time = c(service_time,sum(info_instance$service_times_df$ServiceTime[info_instance$service_times_df$ID %in% nodos]))
      waiting_time = c(waiting_time,sum(tableOrder$waiting_time))
      charging_time = c(charging_time,sum(tableOrder_aux$charging_times))
      iter = iter + 1
    }
    
    
    finalTable = data.frame(route_id,start_time,driving_time,service_time,waiting_time,charging_time,objective_function = (driving_time+waiting_time+charging_time),energy_consumption)
    vector = c("Total","-",sum(finalTable$driving_time),sum(finalTable$service_time),sum(finalTable$waiting_time),sum(finalTable$charging_time),sum(finalTable$objective_function),sum(finalTable$energy_consumption))
    finalTable = rbind(finalTable,vector)
    rm(info_instance,info_solution)
    return(finalTable)
  }
  
  # For plotting a solution:
  
  createGraph <- function(insNum,algor){
    
    # Load the instance:
    
    info_instance = loadInstance_soft(insNum)
    
    # Load the solution:
    
    info_solution = loadSolution_soft(insNum,algor,info_instance)
    
    # Plot the nodes:
    
    symbs = c()
    cli = length(info_instance$nodes_df$CoorX)-1
    symbs = info_instance$nodes_df$Type
    ids = info_instance$nodes_df$ID
    
    p = plot(info_instance$nodes_df$CoorX,info_instance$nodes_df$CoorY,pch=symbs,xlab = "x coordinate (km)",ylab =  "y coordinate (km)",xlim=c(0,max(info_instance$nodes_df$CoorX)+50),ylim=c(0,max(info_instance$nodes_df$CoorY)+50),xaxt="n",yaxt="n")
    axis(1, at = seq(0, max(info_instance$nodes_df$CoorX)+50, by = 10), las=2)
    axis(2, at = seq(0, max(info_instance$nodes_df$CoorY)+50, by = 10), las=2)
    text(info_instance$nodes_df$CoorX, info_instance$nodes_df$CoorY, ids, cex=0.8, pos=1, col="black")
    
    # Plot the arcs:
    
    colors = c()
    route_ids = c()
    route_id = 1
    for(route in info_solution){
      
      tableOrder = route$order_df
      tableCoors = info_instance$nodes_df
      tableOrder = sqldf("SELECT a.*,b.CoorX AS tail_x,b.CoorY AS tail_y from tableOrder as a
                         LEFT JOIN (SELECT * from tableCoors) as b
                         ON a.tail = b.ID")
      tableOrder = sqldf("SELECT a.*,b.CoorX AS head_x,b.CoorY AS head_y from tableOrder as a
                         LEFT JOIN (SELECT * from tableCoors) as b
                         ON a.head = b.ID")
      color = randomColor(1,luminosity="dark")

      for(i in 1:nrow(tableOrder)){
        p <- p + arrows(x0=tableOrder[i,4],x1=tableOrder[i,6],y0=tableOrder[i,5],y1=tableOrder[i,7],lty=1,col=color,angle=30,length=0.1)
      }
      colors = c(colors,color)
      route_ids = c(route_ids,route_id)
      route_id = route_id + 1
    }
    
    legend(x="topright",legend=route_ids,lty=c(2,1),col=colors,inset=0.05)
    
    # Return the plot:
    rm(info_instance,info_solution)
    return(p)
    
  }
  
  # For a new solutio:
  
  
  #Function to evaluate a solution:
  
  createTableT <- function(insNum,info_sol){
    
    # Load the instance:
    
    info_instance = loadInstance(insNum)
    
    # Load the solution:
    
    info_solution = loadSolutionT(insNum,info_sol,info_instance)
    
    # Calculate the total time for each route:
    
    route_id = c()
    driving_time = c()
    charging_time = c()
    service_time = c()
    waiting_time = c()
    energy_consumption = c()
    start_time = c()
    iter = 1
    for(route in info_solution){
      tableResources = info_instance$resources_matrix_df
      tableOrder = route$order_df
      tableMix = tableResources[tableResources$key %in% unique(tableOrder$key),]
      
      nodos = unique(c(tableOrder$tail,tableOrder$head))
      
      tableOrder_aux = sqldf("SELECT a.*,b.energy,b.time from tableOrder as a LEFT JOIN 
                             (SELECT key,energy,time FROM tableResources) as b
                             on a.key = b.key")
      
      energy_vec = c()
      energy_vec = c(energy_vec,info_instance$battery_capacity)
      charging_times_f = c()
      for(i in 1:nrow(tableOrder_aux)){
        
        if(tableOrder_aux$cs_type[i] == "NotACS"){
          
          energy_vec = c(energy_vec,energy_vec[length(energy_vec)] - tableOrder_aux$energy[i])
          charging_times_f = c(charging_times_f,0)
          
        }else{
          
          charging_times_f = c(charging_times_f,calculateChargingTime(info_instance$charging_times[[tableOrder_aux$cs_type[i]]],info_instance$battery_points[[tableOrder_aux$cs_type[i]]],energy_vec[length(energy_vec)],tableOrder_aux$charge_time[i]))
          energy_vec = c(energy_vec,energy_vec[length(energy_vec)] - tableOrder_aux$energy[i] + tableOrder_aux$charge_time[i])
        }
        
      }
      
      tableOrder_aux$energy_at_each_time = energy_vec[2:length(energy_vec)]
      tableOrder_aux$charging_times = charging_times_f
      
      # Store them all: 
      
      start_time = c(start_time,route$start_time)
      energy_consumption = c(energy_consumption,sum(tableMix$energy))
      driving_time = c(driving_time,sum(tableMix$time))
      route_id = c(route_id,iter)
      service_time = c(service_time,sum(info_instance$service_times_df$ServiceTime[info_instance$service_times_df$ID %in% nodos]))
      waiting_time = c(waiting_time,sum(tableOrder$waiting_time))
      charging_time = c(charging_time,sum(tableOrder_aux$charging_times))
      iter = iter + 1
    }
    
    
    finalTable = data.frame(route_id,start_time,driving_time,service_time,waiting_time,charging_time,objective_function = (driving_time+waiting_time+charging_time),energy_consumption)
    vector = c("Total","-",sum(finalTable$driving_time),sum(finalTable$service_time),sum(finalTable$waiting_time),sum(finalTable$charging_time),sum(finalTable$objective_function),sum(finalTable$energy_consumption))
    finalTable = rbind(finalTable,vector)
    rm(info_instance,info_solution)
    return(finalTable)
  }
  
  
  loadSolutionT <- function(insNum,info_sol,info_instance){
    
    # Read the xml file:
    
    listOfElements <-  info_sol
    
    # Read each route:
    
    info_routes = list()
    
    for(route in listOfElements$solution){
      
      start_time = as.double(route$start_time)
      id_route = as.integer(attributes(route)[["id"]])
      
      # The sequence:
      
      nodes_id = c()
      charging_times = c()
      waiting_times = c()
      cs_types = c()
      currentSoC = info_instance$battery_capacity
      for(node in route$sequence){
        
        if(length(node) > 1){
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])
          type_of_cs = info_instance$nodes_df$TypeOfCS[info_instance$nodes_df$ID == attributes(node)[["id"]]]
          cs_types = c(cs_types,type_of_cs)
          charging_times = c(charging_times,as.double(node$charge))
          waiting_times = c(waiting_times,as.double(node$wait))
          
        }else{
          
          nodes_id = nodes_id = c(nodes_id,attributes(node)[["id"]])
          charging_times = c(charging_times,0)
          waiting_times = c(waiting_times,0)
          cs_types = c(cs_types,"NotACS")
        }
      }
      
      head = nodes_id[2:length(nodes_id)]
      tail = nodes_id[1:(length(nodes_id)-1)]
      
      order_df = data.frame(tail,head,charge_time = charging_times[1:length(tail)],waiting_time = waiting_times[1:length(tail)],key=paste0(tail,";",head),cs_type = cs_types[1:length(tail)])
      list_info_this_route = list()
      list_info_this_route[["start_time"]] = start_time
      list_info_this_route[["order_df"]] = order_df
      info_routes[[paste0("",id_route)]] = list_info_this_route  
      
    }
    rm(listOfElements)
    return(info_routes)
    
    
  }
  
  
  # For plotting a solution:
  
  createGraphT <- function(insNum,info_sol){
    
    # Load the instance:
    
    info_instance = loadInstance_soft(insNum)
    
    # Load the solution:
    
    info_solution = loadSolutionT(insNum,info_sol,info_instance)
    
    # Plot the nodes:
    
    symbs = c()
    cli = length(info_instance$nodes_df$CoorX)-1
    symbs = info_instance$nodes_df$Type
    ids = info_instance$nodes_df$ID
    
    p = plot(info_instance$nodes_df$CoorX,info_instance$nodes_df$CoorY,pch=symbs,xlab = "x coordinate (km)",ylab =  "y coordinate (km)",xlim=c(0,max(info_instance$nodes_df$CoorX)+50),ylim=c(0,max(info_instance$nodes_df$CoorY)+50),xaxt="n",yaxt="n")
    axis(1, at = seq(0, max(info_instance$nodes_df$CoorX)+50, by = 10), las=2)
    axis(2, at = seq(0, max(info_instance$nodes_df$CoorY)+50, by = 10), las=2)
    text(info_instance$nodes_df$CoorX, info_instance$nodes_df$CoorY, ids, cex=0.8, pos=1, col="black")
    
    # Plot the arcs:
    
    colors = c()
    route_ids = c()
    route_id = 1
    for(route in info_solution){
      
      tableOrder = route$order_df
      tableCoors = info_instance$nodes_df
      tableOrder = sqldf("SELECT a.*,b.CoorX AS tail_x,b.CoorY AS tail_y from tableOrder as a
                         LEFT JOIN (SELECT * from tableCoors) as b
                         ON a.tail = b.ID")
      tableOrder = sqldf("SELECT a.*,b.CoorX AS head_x,b.CoorY AS head_y from tableOrder as a
                         LEFT JOIN (SELECT * from tableCoors) as b
                         ON a.head = b.ID")
      color = randomColor(1,luminosity="dark")
      for(i in 1:nrow(tableOrder)){
        p <- p + arrows(x0=tableOrder[i,7],x1=tableOrder[i,9],y0=tableOrder[i,8],y1=tableOrder[i,10],lty=1,col=color,angle=30,length=0.1)
      }
      colors = c(colors,color)
      route_ids = c(route_ids,route_id)
      route_id = route_id + 1
    }
    
    legend(x="topright",legend=route_ids,lty=c(2,1),col=colors,inset=0.05)
    
    # Return the plot:
    rm(info_instance,info_solution)
    return(p)
    
  }
  
  
  #For printing the solution file:
  
  findName <- function(ins){
    
    return(paste("Example-",ins,".xml", sep=""))
  }
  
  findSolution <- function(insNum){
    algor = "BKS"
    if(algor == "BKS"){
      cadena = paste0("/sol_EVRPNL_",algor,"_")
    }
    
    # Read the xml file:
    
    listOfElements <-  read_xml(paste0("./solutions/",algor,cadena,insNum,".xml"))
    
    return(listOfElements)
  }

##### Run the application ####
shinyApp(ui = ui, server = server)


