##Crawl PTT beauty 


library(xml2)
library(rvest)


##__crwal PTT beauty from page 2320 to 2321__________

df<-data.frame()

for(q in 2320:2321){
for(i in 1:length(links)){
  url <- paste0("https://www.ptt.cc/bbs/Beauty/index",q,".html")
  doc   <- read_html(url)
  pre <- "https://www.ptt.cc"
  linkpath <- '//*[@id="main-container"]/div[2]/div/div[3]/a'
  links <- xml_attr(xml_find_all(doc, linkpath), "href")
  links <- paste0(pre, links)
  authorx<-'//*[@id="main-content"]/div[1]/span[2]'
  titlex<-'//*[@id="main-content"]/div[3]/span[2]'
  imgurlx<-'//*[@id="main-content"]/a'
  doc2<-read_html(links[i])
  author<-xml_text(xml_find_all(doc2,authorx))
  title<-xml_text(xml_find_all(doc2,titlex))
  content<-xml_text(xml_find_all(doc2,imgurlx))
  if(length(content)==0){
    next
  }
  tempdf<-data.frame(
    author = author,
    title =title ,
    content = content
  )
  df<-rbind(df,tempdf)
  print(i)
  print(q)
  }
}



###file downloading___________________________________________
for(i in 1:nrow(df)){
  links<-df$content[i]
  download.file(links,paste("D:/beauty/",df$title[i],i,".jpg",sep=""),mode = "wb")
}
###__檔案資料夾路徑和名稱可以自己隨便設定!___________________

