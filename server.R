library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
  read.table(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })

  
  output$title_upload=renderText({
      
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    "Following data has been uploaded successfully."
  })

  output$title1_mce1=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1))
      return(NULL)
    
    paste("C-Index, Gamma and Silhouette Width Graphs from k =",input$k_min," to k =",input$k_max," for Determining Number of Clusters")
  })
  
  output$title2_mce1=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1)|is.null(input$k_best))
      return(NULL)
    
    "Performance of MCE 1: Actual vs. Predicted Cost"
  })
  
  output$title3_mce1=renderText({
    if (is.null(input$var_type)|is.null(input$file1)|is.null(input$k_best))
      return(NULL)
    
    "Processed Dataset and Predicted Values (y_hat)"
  })

  output$title1_mce2=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1))
      return(NULL)
    
    "Performance of MCE 2: Actual vs. Predicted Cost"
  })
  
  output$title2_mce2=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1))
      return(NULL)
    
    "Processed Dataset and Predicted Values (y_hat)"
  })
  
  output$title1_mce3=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1))
      return(NULL)
    
    "Performance of MCE 3: Actual vs. Predicted Cost"
  })
  
  output$title2_mce3=renderText({
    
    if (is.null(input$var_type)|is.null(input$file1))
      return(NULL)
    
    "Processed Dataset and Predicted Values (y_hat)"
  })
  
  output$distPlot <- renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    # Number of clusters
    k_min=input$k_min
    k_max=input$k_max
    
    if (input$red_dot==0){
      red_dot=NULL
    }else{
      red_dot=input$red_dot
    }
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    if (is.null(input$k_min))
      return(NULL)
    
    if (is.null(input$k_min))
      return(NULL)
  
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
      
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    # Load packages
    library(cluster)
    library(MASS)
    library(ade4) 
    library(R2HTML) 
    library(e1071) 
    library(class) 
    library(rgl)
    library(clusterSim)
    library(StatMatch)
    

    if (is.null(k_min))
      return(NULL)
    
    if (is.null(k_max))
      return(NULL)
    
    # Construct Gower's dissimilarity matrix
    gowers_data=data_main
    for(i in 1:(n_var-1)){
      if(variable_type_new[i]==6){
        gowers_data[,i]=as.logical(gowers_data[,i]==1)
      }
    }
    diss=gower.dist(data_main,data_main)
    
    pamx=matrix(0,n_obs,k_max)
    Gamma=matrix(0,k_max)
    Swidth=matrix(0,k_max)
    Cindex=matrix(0,k_max)
    
    for (i in k_min:(k_max)){
      pamx[,i]=pam(diss,i,diss=TRUE)$clustering
    }
    
    for (i in k_min:k_max){
      Gamma[i]=index.G2(diss,pamx[,i])
      Cindex[i]=index.G3(diss,pamx[,i])
      Swidth[i]=index.S(diss,pamx[,i])
    }
    
    par(mfrow=c(1,3))
    plot(Cindex,type="l",ann=FALSE)
    title(ylab="C-index")
    title(xlab="Number of Clusters")
    if (!is.null(red_dot)){
      points(red_dot,Cindex[red_dot],col="red",pch=19)
    }
    
    plot(Gamma,type="l",ann=FALSE)
    title(ylab="Gamma")
    title(xlab="Number of Clusters")
    if (!is.null(red_dot)){
      points(red_dot,Gamma[red_dot],col="red",pch=19)
    }
    

    plot(Swidth,type="l",ann=FALSE)
    title(ylab="Silhouette Width")
    title(xlab="Number of Clusters")
    abline(h=0.5,col="red",lty=2)
    
    if (!is.null(red_dot)){
      points(red_dot,Swidth[red_dot],col="red",pch=19)
    }
    
  })
  

  
  output$cluster_result=renderTable({
    
    # Number of clusters
    
    if (input$k_best==0){
      return(NULL)
    }
      
    k=input$k_best
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
    
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    ###### Clustering - Start ######
    
    # Load packages
    library(cluster)
    library(MASS)
    library(ade4) 
    library(R2HTML) 
    library(e1071) 
    library(class) 
    library(rgl)
    library(clusterSim)
    library(StatMatch)
    
    # Construct Gower's dissimilarity matrix
    
    gowers_data=data_main
    
    for(i in 1:(n_var-1)){
      if(variable_type_new[i]==6){
        gowers_data[,i]=as.logical(gowers_data[,i]==1)
      }
    }
    diss=gower.dist(data_main,data_main)
    
    
    pam_clusters=pam(diss,k,diss=TRUE)$clustering
    pam_medoids=pam(diss,k,diss=TRUE)$medoids
    
    # Build cluster specific regression models
    # Count how many observations in each cluster
    cluster_count=numeric(k)
    
    for(i in 1:k){
      cluster_count[i]=sum(pam_clusters==i)
    }
    
    enumeration=1:n_obs
    pam_clusters_enumerated=cbind(enumeration,pam_clusters)
    
    for(i in 1:k){
      assign(paste('cluster_content_',i,sep=''),pam_clusters_enumerated[which(pam_clusters_enumerated[,2]==i)])
      assign(paste('cluster_data_',i,sep=''),eval(parse(text=paste('data_full[cluster_content_',i,',]',sep=''))))
    }
  
    
    # Regression model
    
    
    p=input$mce1_degree
    
    if(p==3){
      p=input$mce1_poly_degree
    }
    
    variable_factor_count=numeric(n_var)
    
    for(j in 1:k){
      
      for(i in 1:(n_var-1)){
        if(variable_type_new[i]>=3){
          variable_factor_count[i]=length(unique(eval(parse(text=paste('cluster_data_',j,'[,',i,']',sep='')))))
        }
      }
      
      model_expression="y~x1"
      
      if(p>1){
        for(i in 2:p){
          model_expression=paste(model_expression,'+I(x1','^',i,')',sep='')
        }
      }
      
      for(i in 2:x_index){
        model_expression=paste(model_expression,'+x',i,sep='')
        
        if(p>1){
          for(n in 2:p){
            model_expression=paste(model_expression,'+I(x',i,'^',n,')',sep='')
          }
        }
      }
      
      for(i in 1:z_index){
        if(variable_factor_count[x_index+i]>1){
          model_expression=paste(model_expression,'+z',i,sep='')
        }
      }
      
      
      
      model_expression=paste('model_cluster_',j,'=lm(',model_expression,',data=cluster_data_',j,')',sep='')
      eval(parse(text=model_expression))
    }
    
    # Report result
    clustering_result=cbind(cluster_data_1,model_cluster_1$fitted.values,rep(1,nrow(cluster_data_1)))
    names(clustering_result)[n_var+1]="y_hat"
    names(clustering_result)[n_var+2]="cluster"
    
    for(j in 2:k){
      current_cluster_result=cbind(eval(parse(text=paste('cluster_data_',j,sep=''))),eval(parse(text=paste('model_cluster_',j,'$fitted.values',sep=''))),rep(j,nrow(eval(parse(text=paste('cluster_data_',j,sep=''))))))
      names(current_cluster_result)[n_var+1]="y_hat"
      names(current_cluster_result)[n_var+2]="cluster"
      clustering_result=rbind(clustering_result,current_cluster_result)
    }
    
    clustering_result=clustering_result
    
    ###### Clustering - End ######
    
  })
  

  
  output$actual_predicted1=renderPlot({
    
    # Number of clusters
    
    if (input$k_best==0){
      return(NULL)
    }
    
    k=input$k_best
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
    
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    ###### Clustering - Start ######
    
    # Load packages
    library(cluster)
    library(MASS)
    library(ade4) 
    library(R2HTML) 
    library(e1071) 
    library(class) 
    library(rgl)
    library(clusterSim)
    library(StatMatch)
    
    # Construct Gower's dissimilarity matrix
    
    gowers_data=data_main
    
    for(i in 1:(n_var-1)){
      if(variable_type_new[i]==6){
        gowers_data[,i]=as.logical(gowers_data[,i]==1)
      }
    }
    diss=gower.dist(data_main,data_main)
    
    
    pam_clusters=pam(diss,k,diss=TRUE)$clustering
    pam_medoids=pam(diss,k,diss=TRUE)$medoids
    
    # Build cluster specific regression models
    # Count how many observations in each cluster
    cluster_count=numeric(k)
    
    for(i in 1:k){
      cluster_count[i]=sum(pam_clusters==i)
    }
    
    enumeration=1:n_obs
    pam_clusters_enumerated=cbind(enumeration,pam_clusters)
    
    for(i in 1:k){
      assign(paste('cluster_content_',i,sep=''),pam_clusters_enumerated[which(pam_clusters_enumerated[,2]==i)])
      assign(paste('cluster_data_',i,sep=''),eval(parse(text=paste('data_full[cluster_content_',i,',]',sep=''))))
    }
    
    
    
    # Regression model
    
    p=input$mce1_degree
    
    if(p==3){
      p=input$mce1_poly_degree
    }
    
    variable_factor_count=numeric(n_var)
    
    for(j in 1:k){
      
      for(i in 1:(n_var-1)){
        if(variable_type_new[i]>=3){
          variable_factor_count[i]=length(unique(eval(parse(text=paste('cluster_data_',j,'[,',i,']',sep='')))))
        }
      }
      
      model_expression="y~x1"
      
      if(p>1){
        for(i in 2:p){
          model_expression=paste(model_expression,'+I(x1','^',i,')',sep='')
        }
      }
      
      for(i in 2:x_index){
        model_expression=paste(model_expression,'+x',i,sep='')
        
        if(p>1){
          for(n in 2:p){
            model_expression=paste(model_expression,'+I(x',i,'^',n,')',sep='')
          }
        }
      }
      
      for(i in 1:z_index){
        if(variable_factor_count[x_index+i]>1){
          model_expression=paste(model_expression,'+z',i,sep='')
        }
      }
        
        model_expression=paste('model_cluster_',j,'=lm(',model_expression,',data=cluster_data_',j,')',sep='')
        eval(parse(text=model_expression))
      }
      
      # Report result
      clustering_result=cbind(cluster_data_1,model_cluster_1$fitted.values,rep(1,nrow(cluster_data_1)))
      names(clustering_result)[n_var+1]="y_hat"
      names(clustering_result)[n_var+2]="cluster"
      
      for(j in 2:k){
        current_cluster_result=cbind(eval(parse(text=paste('cluster_data_',j,sep=''))),eval(parse(text=paste('model_cluster_',j,'$fitted.values',sep=''))),rep(j,nrow(eval(parse(text=paste('cluster_data_',j,sep=''))))))
        names(current_cluster_result)[n_var+1]="y_hat"
        names(current_cluster_result)[n_var+2]="cluster"
        clustering_result=rbind(clustering_result,current_cluster_result)
      }
      
      plot_result=clustering_result[order(clustering_result$y),]
      
      plot(plot_result$y,type="n",col="blue",ylab="Cost",xlab="Products")
      lines(plot_result$y, col="blue")
      lines(plot_result$y_hat, col="red")
      legend("bottomright", c("Actual","Predicted"), cex=0.8, 
             col=c("blue","red"), lty=1:1);
      
      ###### Clustering - End ######

  })
  
  output$actual_predicted2=renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
    
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    # load CRS package
    library(crs)
    
    model_expression="y~x1"
    
    for(i in 2:x_index){
      model_expression=paste(model_expression,'+x',i,sep='')
    }
    
    for(i in 1:z_index){
      model_expression=paste(model_expression,'+z',i,sep='')
    }
    
    model_expression=paste("crs(",model_expression,",basis=\"",input$basis,"\",data.return=TRUE,model.return=TRUE,degree.min=",input$degree_min,",degree.max=",input$degree_max,",knots=\"",input$knots,"\",cv=\"",input$cv,"\",cv.func=\"",input$cv.func,"\",segments.min=",input$segment_min,",segments.max=",input$segment_max,",complexity=\"",input$complexity,"\")",sep="")
    splines_model=eval(parse(text=model_expression))
    
    #predict(model,newdata=data_frame)
    #plot(model,mean=TRUE)
    
    # Report result
    splines_result=cbind(data_full,splines_model$fitted.values)
    names(splines_result)[n_var+1]="y_hat"
    
    plot_result=splines_result[order(splines_result$y),]
    
    plot(plot_result$y,type="n",col="blue",ylab="Cost",xlab="Products")
    lines(plot_result$y, col="blue")
    lines(plot_result$y_hat, col="red")
    legend("bottomright", c("Actual","Predicted"), cex=0.8, 
           col=c("blue","red"), lty=1:1);
    
  })
  
  output$spline_result=renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
    
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    # load CRS package
    library(crs)
    
    model_expression="y~x1"
    
    for(i in 2:x_index){
      model_expression=paste(model_expression,'+x',i,sep='')
    }
    
    for(i in 1:z_index){
      model_expression=paste(model_expression,'+z',i,sep='')
    }
    
    model_expression=paste("crs(",model_expression,",basis=\"",input$basis,"\",data.return=TRUE,model.return=TRUE,degree.min=",input$degree_min,",degree.max=",input$degree_max,",knots=\"",input$knots,"\",cv=\"",input$cv,"\",cv.func=\"",input$cv.func,"\",segments.min=",input$segment_min,",segments.max=",input$segment_max,",complexity=\"",input$complexity,"\")",sep="")
    splines_model=eval(parse(text=model_expression))
    
    #predict(model,newdata=data_frame)
    #plot(model,mean=TRUE)
    
    # Report result
    splines_result=cbind(data_full,splines_model$fitted.values)
    names(splines_result)[n_var+1]="y_hat"
    
    splines_result=splines_result
    
  })
  
  
  
  output$actual_predicted3=renderPlot({
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote)
      
      
      n_var=ncol(data)
      n_obs=nrow(data)
      
      variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
      
      if (is.null(variable_type))
        return(NULL)
      
      x_index=1
      z_index=1
      
      for(i in 1:n_var){
        current_variable=variable_type[i]
        if(current_variable==1 | current_variable==2){
          assign(paste('x',x_index,sep=''),data[,i])
          x_index=x_index+1
        } else if(current_variable==3 | current_variable==5 | current_variable==6){
          assign(paste('z',z_index,sep=''),data[,i])
          assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
          z_index=z_index+1
        } else if(current_variable==4) {
          assign(paste('z',z_index,sep=''),data[,i])
          assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
          z_index=z_index+1
        } else if(current_variable==0) {
          y=data[,i]
        }
      }
      
      # Combine data in a matrix
      x_index=x_index-1
      z_index=z_index-1
      
      variable_type_new=variable_type
      current_x=0
      current_z=0
      
      for(i in 1:(n_var-1)){
        current_variable=variable_type[i]
        if(current_variable==1 | current_variable==2){
          current_x=current_x+1
          variable_type_new[current_x]=current_variable
        }else{
          current_z=current_z+1
          variable_type_new[x_index+current_z]=current_variable
        }
      }
      
      
      bind_expression="d_x1"
      d_x1=data.frame(x1)
      
      if(x_index==1){ 
        for(i in 1:z_index){
          assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
          eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
          bind_expression=paste(bind_expression,',d_z',i,sep='')
        }
      }else{
        for(i in 2:x_index){
          assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
          eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
          bind_expression=paste(bind_expression,',d_x',i,sep='')
        }
        for(i in 1:z_index){
          assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
          eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
          bind_expression=paste(bind_expression,',d_z',i,sep='')
        }
      }
      
      bind_expression=paste("cbind(",bind_expression,")",sep="")
      data_main=eval(parse(text=bind_expression))
      
      d_y=data.frame(y)
      data_full=cbind(data_main,d_y)
      
      for(i in 1:x_index){
        eval(parse(text=paste('rm(d_x',i,')',sep='')))
      }
      
      for(i in 1:z_index){
        eval(parse(text=paste('rm(d_z',i,')',sep='')))
      }
      
      rm(d_y)
      rm(bind_expression)
      
      
        ###### No clustering - Start ######
        
        p=input$mce3_degree
      
      if(p==3){
        p=input$mce3_poly_degree
      }
        
        if (p==1){
          model_expression="y~x1"
          
          for(i in 2:x_index){
            model_expression=paste(model_expression,'+x',i,sep='')
          }
          
          for(i in 1:z_index){
            model_expression=paste(model_expression,'+z',i,sep='')
          }
        }
        
        if (p>1){
          model_expression="y~x1+I(x1^2)"
          
          for(j in 2:p){
            model_expression=paste(model_expression,'+I(x1','^',j,')',sep='')
          }
          
          for(i in 2:x_index){
            model_expression=paste(model_expression,'+x',i,sep='')
            
            for(j in 2:p){
              model_expression=paste(model_expression,'+I(x',i,'^',j,')',sep='')
            }
          }
          
          for(i in 1:z_index){
            model_expression=paste(model_expression,'+z',i,sep='')
          }
        }
        
        
        model_expression_1=paste("lm(",model_expression,",data=data_full)",sep="")
        
        noCluster_regression1=eval(parse(text=model_expression_1))
        
        noCluster_result=cbind(data_full,noCluster_regression1$fitted.values)
        names(noCluster_result)[n_var+1]="y_hat"
        
        plot_result=noCluster_result[order(noCluster_result$y),]
        
        plot(plot_result$y,type="n",col="blue",ylab="Cost",xlab="Products")
        lines(plot_result$y, col="blue")
        lines(plot_result$y_hat, col="red")
        legend("bottomright", c("Actual","Predicted"), cex=0.8, 
               col=c("blue","red"), lty=1:1);
        
        ###### No clustering - End ######
    })
  
  output$nocluster_result=renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data=read.table(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
    
    
    n_var=ncol(data)
    n_obs=nrow(data)
    
    variable_type=eval(parse(text=paste('c(',input$var_type,')',sep='')))
    
    if (is.null(variable_type))
      return(NULL)
    
    x_index=1
    z_index=1
    
    for(i in 1:n_var){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        assign(paste('x',x_index,sep=''),data[,i])
        x_index=x_index+1
      } else if(current_variable==3 | current_variable==5 | current_variable==6){
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),factor(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==4) {
        assign(paste('z',z_index,sep=''),data[,i])
        assign(paste('z',z_index,sep=''),ordered(eval(parse(text=paste('z',z_index,sep='')))))
        z_index=z_index+1
      } else if(current_variable==0) {
        y=data[,i]
      }
    }
    
    # Combine data in a matrix
    x_index=x_index-1
    z_index=z_index-1
    
    variable_type_new=variable_type
    current_x=0
    current_z=0
    
    for(i in 1:(n_var-1)){
      current_variable=variable_type[i]
      if(current_variable==1 | current_variable==2){
        current_x=current_x+1
        variable_type_new[current_x]=current_variable
      }else{
        current_z=current_z+1
        variable_type_new[x_index+current_z]=current_variable
      }
    }
    
    
    bind_expression="d_x1"
    d_x1=data.frame(x1)
    
    if(x_index==1){ 
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }else{
      for(i in 2:x_index){
        assign(paste('d_x',i,sep=''),data.frame(eval(parse(text=paste('x',i,sep='')))))
        eval(parse(text=paste('names(d_x',i,')="x',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_x',i,sep='')
      }
      for(i in 1:z_index){
        assign(paste('d_z',i,sep=''),data.frame(eval(parse(text=paste('z',i,sep='')))))
        eval(parse(text=paste('names(d_z',i,')="z',i,'"',sep='')))
        bind_expression=paste(bind_expression,',d_z',i,sep='')
      }
    }
    
    bind_expression=paste("cbind(",bind_expression,")",sep="")
    data_main=eval(parse(text=bind_expression))
    
    d_y=data.frame(y)
    data_full=cbind(data_main,d_y)
    
    for(i in 1:x_index){
      eval(parse(text=paste('rm(d_x',i,')',sep='')))
    }
    
    for(i in 1:z_index){
      eval(parse(text=paste('rm(d_z',i,')',sep='')))
    }
    
    rm(d_y)
    rm(bind_expression)
    
    
    p=input$mce3_degree
    
    if(p==3){
      p=input$mce3_poly_degree
    }
    
    if (p==1){
      model_expression="y~x1"
      
      for(i in 2:x_index){
        model_expression=paste(model_expression,'+x',i,sep='')
      }
      
      for(i in 1:z_index){
        model_expression=paste(model_expression,'+z',i,sep='')
      }
    }
    
    if (p>1){
      model_expression="y~x1+I(x1^2)"
      
      for(j in 2:p){
        model_expression=paste(model_expression,'+I(x1','^',j,')',sep='')
      }
      
      for(i in 2:x_index){
        model_expression=paste(model_expression,'+x',i,sep='')
        
        for(j in 2:p){
          model_expression=paste(model_expression,'+I(x',i,'^',j,')',sep='')
        }
      }
      
      for(i in 1:z_index){
        model_expression=paste(model_expression,'+z',i,sep='')
      }
    }
      
      model_expression_1=paste("lm(",model_expression,",data=data_full)",sep="")
      
      noCluster_regression1=eval(parse(text=model_expression_1))
      
      noCluster_result=cbind(data_full,noCluster_regression1$fitted.values)
      names(noCluster_result)[n_var+1]="y_hat"
      
      noCluster_result=noCluster_result
      
      ###### No clustering - End ######

    
  })
  
})


