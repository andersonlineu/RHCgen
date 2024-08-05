#' Converte os Tipos Histológicos para os Nomes Completos.
#'
#' Esta função mapeia os códigos de Tipo Histológico para os nomes completos na coluna `Tipo_Histologico` de um dataframe. Os nomes completos dos tipos histológicos são adicionados em uma nova coluna `Tipo_Histologico_Completo`.
#'
#' @param dados Um dataframe contendo a coluna `Tipo_Histologico`.
#' @return Retorna um dataframe com os nomes completos dos tipos histológicos adicionados em uma nova coluna.
#' @export
#' @name renomear_tipo_histologico
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- renomear_tipo_histologico(dados_RHC_combinados)
renomear_tipo_histologico <- function(dados) {
  message("Iniciando o ajuste dos códigos de Tipo Histológico para nomes completos.")

  # Definição do dataframe de mapeamento de Tipo Histológico para nomes completos
  tipo_histologico_names <- data.frame(
    Codigo_tipo_hist = c(
      "8000/0", "8000/1", "8000/3", "8000/6", "8001/0", "8001/1", "8001/3", "8002/3", "8003/3", "8004/3",
      "8010/0", "8010/2", "8010/3", "8010/6", "8011/0", "8011/3", "8012/3", "8020/3", "8021/3", "8022/3",
      "8030/3", "8031/3", "8032/3", "8033/3", "8034/3", "8040/1", "8041/3", "8042/3", "8043/3", "8044/3",
      "8045/3", "8050/0", "8050/2", "8050/3", "8051/0", "8051/3", "8052/0", "8052/3", "8053/0", "8060/0",
      "8070/2", "8070/3", "8070/6", "8071/3", "8072/3", "8073/3", "8074/3", "8075/3", "8076/2", "8076/3",

      "8077/2", "8080/2", "8081/2", "8082/3", "8090/1", "8090/3", "8091/3", "8092/3", "8093/3", "8094/3",
      "8095/3", "8096/0", "8100/0", "8101/0", "8102/0", "8110/0", "8110/3", "8120/0", "8120/1", "8120/2",
      "8120/3", "8121/0", "8121/1", "8121/3", "8122/3", "8123/3", "8124/3", "8130/3", "8140/0", "8140/1",
      "8140/2", "8140/3", "8140/6", "8141/3", "8142/3", "8143/3", "8144/3", "8145/3", "8146/0", "8147/0",
      "8147/3", "8150/0", "8150/3", "8151/0", "8151/3", "8152/0", "8152/3", "8153/1", "8153/3", "8154/3",

      "8155/3", "8160/0", "8160/3", "8161/0", "8161/3", "8162/3", "8170/0", "8170/3", "8171/3", "8180/3",
      "8190/0", "8190/3", "8191/0", "8200/0", "8200/3", "8201/3", "8202/0", "8210/0", "8210/2", "8210/3",
      "8211/0", "8211/3", "8220/0", "8220/3", "8221/0", "8221/3", "8230/3", "8231/3", "8240/1", "8240/3",
      "8241/1", "8241/3", "8243/3", "8244/3", "8245/3", "8246/3", "8247/3", "8248/1", "8250/1", "8250/3",
      "8251/0", "8251/3", "8260/0", "8260/3", "8261/1", "8261/2", "8261/3", "8262/3", "8263/0", "8263/2",

      "8263/3", "8270/0", "8270/3", "8271/0", "8280/0", "8280/3", "8281/0", "8281/3", "8290/0", "8290/3",
      "8300/0", "8300/3", "8310/0", "8310/3", "8311/1", "8312/3", "8313/0", "8314/3", "8315/3", "8320/3",
      "8321/0", "8322/0", "8322/3", "8323/0", "8323/3", "8324/0", "8330/0", "8330/3", "8331/3", "8332/3",
      "8333/0", "8334/0", "8340/3", "8350/3", "8360/1", "8361/1", "8370/0", "8370/3", "8371/0", "8372/0",
      "8373/0", "8374/0", "8375/0", "8380/0", "8380/1", "8380/3", "8381/0", "8381/1", "8381/3", "8390/0",

      "8390/3", "8400/0", "8400/1", "8400/3", "8401/0", "8401/3", "8402/0", "8403/0", "8404/0", "8405/0",
      "8406/0", "8407/0", "8408/0", "8410/0", "8410/3", "8420/0", "8420/3", "8430/1", "8430/3", "8440/0",
      "8440/3", "8441/0", "8441/3", "8442/3", "8450/0", "8450/3", "8451/3", "8452/1", "8460/0", "8460/3",
      "8461/0", "8461/3", "8462/3", "8470/0", "8470/3", "8471/0", "8471/3", "8472/3", "8473/3", "8480/0",
      "8480/3", "8480/6", "8481/3", "8490/3", "8490/6", "8500/2", "8500/3", "8501/2", "8501/3", "8502/3",

      "8503/0", "8503/2", "8503/3", "8504/0", "8504/2", "8504/3", "8505/0", "8506/0", "8510/3", "8511/3",
      "8512/3", "8520/2", "8520/3", "8521/3", "8522/2", "8522/3", "8530/3", "8540/3", "8541/3", "8542/3",
      "8543/3", "8550/0", "8550/1", "8551/0", "8560/3", "8561/0", "8562/3", "8570/3", "8571/3", "8572/3",
      "8573/3", "8580/0", "8580/3", "8590/1", "8600/0", "8600/3", "8601/3", "8602/0", "8610/0", "8620/1",
      "8620/3", "8621/1", "8622/1", "8623/1", "8630/0", "8630/1", "8630/3", "8631/0", "8632/1", "8640/0",

      "8640/3", "8641/0", "8650/0", "8650/1", "8650/3", "8660/0", "8670/0", "8671/0", "8680/1", "8680/3",
      "8681/1", "8682/1", "8683/0", "8690/1", "8691/1", "8692/1", "8693/1", "8693/3", "8700/0", "8700/3",
      "8710/3", "8711/0", "8712/3", "8713/0", "8720/0", "8720/2", "8720/3", "8721/3", "8722/0", "8722/3",
      "8723/0", "8723/3", "8724/0", "8725/0", "8726/0", "8727/0", "8730/0", "8730/3", "8740/0", "8740/3",
      "8741/2", "8741/3", "8742/2", "8742/3", "8743/3", "8744/3", "8745/3", "8750/0", "8760/0", "8761/1",

      "8761/3", "8770/0", "8770/3", "8771/0", "8771/3", "8772/0", "8772/3", "8773/3", "8774/3", "8780/0",
      "8780/3", "8790/0", "8800/0", "8800/3", "8800/6", "8801/3", "8802/3", "8803/3", "8804/3", "8810/0",
      "8810/3", "8811/0", "8811/3", "8812/0", "8812/3", "8813/0", "8813/3", "8814/3", "8820/0", "8821/1",
      "8822/1", "8823/1", "8824/1", "8830/0", "8830/1", "8830/3", "8832/0", "8832/3", "8833/3", "8840/0",
      "8840/3", "8841/1", "8850/0", "8850/3", "8851/0", "8851/3", "8852/0", "8852/3", "8853/3", "8854/0",

      "8854/3", "8855/3", "8856/0", "8857/0", "8858/3", "8860/0", "8861/0", "8870/0", "8880/0", "8881/0",
      "8890/0", "8890/1", "8890/3", "8891/0", "8892/0", "8893/0", "8894/0", "8894/3", "8895/0", "8895/3",
      "8896/3", "8897/1", "8900/0", "8900/3", "8901/3", "8902/3", "8903/0", "8904/0", "8910/3", "8920/3",
      "8930/0", "8930/3", "8931/1", "8932/0", "8933/3", "8940/0", "8940/3", "8941/3", "8950/3", "8951/3",
      "8960/1", "8960/3", "8963/3", "8964/3", "8970/3", "8971/3", "8972/3", "8980/3", "8981/3", "8982/0",

      "8990/0", "8990/1", "8990/3", "8991/3", "9000/0", "9000/1", "9000/3", "9010/0", "9011/0", "9012/0",
      "9013/0", "9014/0", "9015/0", "9016/0", "9020/0", "9020/1", "9020/3", "9030/3", "9040/0", "9040/3",
      "9041/3", "9042/3", "9043/3", "9044/3", "9050/0", "9050/3", "9051/0", "9051/3", "9052/0", "9052/3",
      "9053/0", "9053/3", "9054/0", "9055/1", "9060/3", "9061/3", "9062/3", "9063/3", "9064/3", "9070/3",
      "9071/3", "9072/3", "9073/1", "9080/0", "9080/1", "9080/3", "9081/3", "9082/3", "9083/3", "9084/0",

      "9084/3", "9085/3", "9090/0", "9090/3", "9091/1", "9100/0", "9100/1", "9100/3", "9101/3", "9102/3",
      "9103/0", "9104/1", "9110/0", "9110/1", "9110/3", "9120/0", "9120/3", "9121/0", "9122/0", "9123/0",
      "9124/3", "9125/0", "9126/0", "9130/0", "9130/1", "9130/3", "9131/0", "9132/0", "9133/1", "9133/3",
      "9134/1", "9140/3", "9141/0", "9142/0", "9150/0", "9150/1", "9150/3", "9160/0", "9161/1", "9170/0",
      "9170/3", "9171/0", "9172/0", "9173/0", "9174/0", "9174/1", "9175/0", "9180/0", "9180/3", "9181/3",

      "9182/3", "9183/3", "9184/3", "9185/3", "9190/3", "9191/0", "9200/0", "9200/1", "9210/0", "9210/1",
      "9220/0", "9220/1", "9220/3", "9221/0", "9221/3", "9230/0", "9230/3", "9231/3", "9240/3", "9241/0",
      "9250/1", "9250/3", "9251/1", "9251/3", "9260/3", "9261/3", "9262/0", "9270/0", "9270/1", "9270/3",
      "9271/0", "9272/0", "9273/0", "9274/0", "9275/0", "9280/0", "9281/0", "9282/0", "9290/0", "9290/3",
      "9300/0", "9301/0", "9302/0", "9310/0", "9310/3", "9311/0", "9312/0", "9320/0", "9321/0", "9322/0",

      "9330/0", "9330/3", "9340/3", "9350/1", "9360/1", "9361/1", "9362/3", "9363/0", "9364/3", "9370/0",
      "9380/3", "9381/3", "9382/3", "9383/1", "9384/1", "9390/0", "9390/3", "9391/3", "9392/3", "9393/1",
      "9394/1", "9400/3", "9401/3", "9410/3", "9411/3", "9420/3", "9421/3", "9422/3", "9423/3", "9424/3",
      "9430/0", "9440/3", "9441/3", "9442/3", "9443/3", "9450/3", "9451/3", "9460/3", "9470/3", "9471/3",
      "9472/3", "9473/3", "9480/3", "9481/3", "9490/0", "9490/3", "9491/3", "9500/3", "9501/3", "9502/3",

      "9503/3", "9504/3", "9505/1", "9506/0", "9507/0", "9510/3", "9511/3", "9512/3", "9520/3", "9521/3",
      "9522/3", "9523/3", "9530/0", "9530/1", "9530/3", "9531/0", "9532/0", "9533/0", "9534/0", "9535/0",
      "9536/0", "9537/0", "9538/1", "9539/3", "9540/0", "9540/1", "9540/3", "9541/0", "9550/0", "9560/0",
      "9560/1", "9560/3", "9561/3", "9562/0", "9570/0", "9580/0", "9580/3", "9581/3", "9590/3", "9591/3",
      "9592/3", "9593/3", "9594/3", "9595/3", "9650/3", "9652/3", "9653/3", "9654/3", "9655/3", "9657/3",

      "9658/3", "9659/3", "9660/3", "9661/3", "9662/3", "9663/3", "9664/3", "9665/3", "9666/3", "9667/3",
      "9670/3", "9671/3", "9672/3", "9673/3", "9674/3", "9675/3", "9676/3", "9677/3", "9680/3", "9681/3",
      "9682/3", "9683/3", "9684/3", "9685/3", "9686/3", "9687/3", "9690/3", "9691/3", "9692/3", "9693/3",
      "9694/3", "9695/3", "9696/3", "9697/3", "9698/3", "9700/3", "9701/3", "9702/3", "9703/3", "9704/3",
      "9705/3", "9706/3", "9707/3", "9709/3", "9711/3", "9712/3", "9713/3", "9714/3", "9720/3", "9722/3",

      "9723/3", "9731/3", "9732/3", "9740/1", "9740/3", "9741/3", "9760/3", "9761/3", "9762/3", "9763/3",
      "9764/3", "9765/1", "9766/1", "9767/1", "9768/1", "9800/3", "9801/3", "9802/3", "9803/3", "9804/3",
      "9820/3", "9821/3", "9822/3", "9823/3", "9824/3", "9825/3", "9826/3", "9827/3", "9830/3", "9840/3",
      "9841/3", "9842/3", "9850/3", "9860/3", "9861/3", "9862/3", "9863/3", "9864/3", "9866/3", "9867/3",
      "9868/3", "9870/3", "9880/3", "9890/3", "9891/3", "9892/3", "9893/3", "9894/3", "9900/3", "9910/3",

      "9930/3", "9931/3", "9932/3", "9940/3", "9941/3", "9950/1", "9960/1", "9961/3", "9962/1", "9970/1",
      "9980/1", "9981/1", "9982/1", "9983/1", "9984/1", "9989/1",
      "8000/9",	"8005/3",	"8010/9",	"8013/3",	"8014/3",	"8015/3",	"8035/3",	"8046/3",	"8052/2",	"8078/3",	"8083/3",	"8084/3",	"8097/3",	"8098/3",	"8102/3",	"8130/1",	"8130/2",	"8131/3",	"8148/2",	"8150/1",	"8156/1",	"8172/3",	"8173/3",	"8174/3",	"8175/3",	"8201/2",	"8214/3",	"8215/3",	"8230/2",	"8242/1",	"8242/3",	"8245/1",	"8249/3",	"8252/3",	"8253/3",	"8254/3",	"8255/3",	"8272/3",	"8313/3",	"8316/3",	"8317/3",	"8318/3",	"8319/3",	"8330/1",	"8333/3",	"8335/3",	"8337/3",	"8341/3",	"8342/3",	"8343/3",	"8344/3",	"8345/3",	"8346/3",	"8347/3",	"8382/3",	"8383/3",	"8384/3",	"8402/3",	"8403/3",	"8407/3",	"8408/3",	"8409/3",	"8413/3",	"8442/1",	"8444/1",	"8451/1",	"8452/3",	"8453/1",	"8453/2",	"8453/3",	"8462/1",	"8463/1",	"8470/2",	"8472/1",	"8473/1",	"8482/3",	"8507/2",	"8508/3",	"8513/3",	"8514/3",	"8523/3",	"8524/3",	"8525/3",	"8550/3",	"8551/3",	"8574/3",	"8575/3",	"8576/3",	"8580/1",	"8581/1",	"8581/3",	"8582/1",	"8582/3",	"8583/1",	"8583/3",	"8584/1",	"8584/3",	"8585/1",	"8585/3",	"8586/3",	"8588/3",	"8589/3",	"8591/1",	"8592/1",	"8593/1",	"8631/1",	"8631/3",	"8633/1",	"8634/3",	"8640/1",	"8642/1",	"8670/3",	"8711/3",	"8728/3",	"8744/2",	"8746/3",
      "8800/9",	"8805/3",	"8806/3",	"8810/1",	"8815/3",	"8825/1",	"8825/3",	"8835/1",	"8836/1",	"8850/1",	"8888/8",	"8891/3",	"8898/1",	"8912/3",	"8921/3",	"8931/3",	"8934/3",	"8935/1",	"8935/3",	"8936/1",	"8936/3",	"8959/1",	"8959/3",	"8973/3",	"8974/1",	"8982/3",	"9014/1",	"9014/3",	"9015/1",	"9015/3",	"9064/2",	"9065/3",	"9105/3",	"9186/3",	"9187/3",	"9192/3",	"9193/3",	"9194/3",	"9242/3",	"9243/3",	"9252/3",	"9351/1",	"9352/1",	"9365/3",	"9370/3",	"9371/3",	"9372/3",	"9390/1",	"9393/3",	"9412/1",	"9413/0",	"9421/1",	"9430/3",	"9442/1",	"9444/1",	"9474/3",	"9505/3",	"9506/1",	"9508/3",	"9513/3",	"9538/3",	"9539/1",	"9571/3",	"9596/3",	"9651/3",	"9678/3",	"9679/3",	"9689/3",	"9699/3",	"9708/3",	"9716/3",	"9717/3",	"9718/3",	"9719/3",	"9727/3",	"9728/3",	"9729/3",	"9733/3",	"9734/3",	"9742/3",	"9750/3",	"9751/1",	"9751/3",	"9752/1",	"9753/1",	"9754/3",	"9755/3",	"9756/3",	"9757/3",	"9758/3",	"9769/1",	"9805/3",	"9831/1",	"9832/3",	"9833/3",	"9834/3",	"9835/3",	"9836/3",	"9837/3",	"9871/3",	"9872/3",	"9873/3",	"9874/3",	"9875/3",	"9876/3",	"9895/3",	"9896/3",	"9897/3",	"9920/3",	"9945/3",	"9946/3",	"9948/3",	"9950/3",	"9960/3",	"9962/3",	"9963/3",
      "9964/3",	"9975/1",	"9980/3",	"9982/3",	"9983/3",	"9984/3",	"9985/3",	"9986/3",	"9987/3",	"9989/3",	"9990/3",
      "8000/0",	"8005/0",	"8152/1",	"8156/3",	"8272/0",	"8313/1",	"8408/1",	"8634/1",	"8728/1",	"8762/1",	"8825/0",	"8827/1",	"8834/1",	"8857/3",	"8936/0",	"9135/1",	"9136/1",	"9195/3",	"9342/3",	"9491/0",	"9571/0",	"9715/3",	"9961/1",	"9990/1",	"9990/9"


    ),
    Tipo_Histologico_Completo = c(
      "Neoplasia benigna", "Neoplasia de comportamento incerto se benigno ou maligno", "Neoplasia maligna", "Neoplasia metastática",
      "Células tumorais, benignas", "Células tumorais, incerto se benignas ou malignas", "Células tumorais malignas", "Tumor maligno, tipo de células pequenas",
      "Tumor maligno, tipo de células gigantes", "Tumor maligno, tipo de células fusiformes", "Tumor epitelial benigno", "Carcinoma in situ SOE",
      "Carcinoma SOE", "Carcinoma metastático SOE", "Epitelioma benigno", "Epitelioma maligno", "Carcinomas de células grandes SOE", "Carcinoma indiferenciado SOE",
      "Carcinoma anaplásico SOE", "Carcinoma pleomórfico", "Carcinoma de células gigantes e de células fusiformes", "Carcinoma de células gigantes",
      "Carcinoma de células fusiformes", "Carcinoma pseudossarcomatoso", "Carcinoma de células poligonais", "Tumorlet", "Carcinoma de células pequenas SOE",
      "Carcinoma 'oat cell'", "Carcinoma de células pequenas, fusiformes", "Carcinoma de células pequenas, intermediárias", "Carcinoma de células pequenas e de células grandes",
      "Papiloma SOE (exceto Papiloma de bexiga M8120/1)", "Carcinoma papilar in situ", "Carcinoma papilar SOE", "Papiloma verrucoso", "Carcinoma verrucoso SOE",
      "Papiloma de células escamosas", "Carcinoma papilar de células escamosas", "Papiloma invertido", "Papilomatose SOE", "Carcinoma in situ de células escamosas, SOE",
      "Carcinoma de células escamosas SOE", "Carcinoma de células escamosas, metastático, SOE", "Carcinoma de células escamosas, queratinizado, SOE",
      "Carcinoma de células escamosas, de células grandes, não queratinizado", "Carcinoma de células escamosas, de células pequenas, não queratinizado",
      "Carcinoma de células escamosas, de células fusiformes", "Carcinoma de células escamosas adenóides", "Carcinoma in situ de células escamosas com invasão questionável do estroma",
      "Carcinoma de células escamosas, microinvasivo", "Neoplasia intra-epitelial, grau III, de colo uterino, vulva e vagina", "Eritroplasia de Queyrat",

      "Doença de Bowen", "Carcinoma linfoepitelial", "Tumor de células basais", "Carcinoma de células basais SOE", "Carcinoma de células basais, multicêntrico",
      "Carcinoma de células basais, tipo morféia", "Carcinoma de células basais, fibroepitelial", "Carcinoma basoescamoso", "Carcinoma metatípico",
      "Epitelioma intra-epidérmico de Jadassohn", "Tricoepitelioma", "Tricofoliculoma", "Tricolemoma", "Pilomatrixoma SOE", "Carcinoma da pilomátrix",
      "Papiloma de células de transição, SOE", "Papiloma urotelial", "Carcinoma in situ de células transicionais", "Carcinoma de células transicionais SOE",
      "Papiloma schneideriano", "Papiloma de células transicionais, tipo invertido", "Carcinoma schneideriano", "Carcinoma de células transicionais, tipo células fusiformas",
      "Carcinoma basalóide", "Carcinoma cloacogênico", "Carcinoma papilar de células transicionais", "Adenoma SOE", "Adenoma brônquico SOE", "Adenocarcinoma in situ SOE",
      "Adenocarcinoma SOE", "Adenocarcinoma metastático SOE", "Adenocarcinoma esquirroso", "Linite plástica", "Adenocarcinoma de propagação superficial",
      "Adenocarcionama, tipo intestinal", "Carcinoma, tipo difuso", "Adenoma monomórfico", "Adenoma de células basais", "Adenocarcinoma de células basais",
      "Adenoma de células das ilhotas", "Carcinoma de células das ilhotas", "Insulinoma SOE", "Insulinoma maligno", "Glucagonoma SOE", "Glucagonoma maligno",
      "Gastrinoma SOE", "Gastrinoma maligno", "Adenocarcinoma misto, das ilhotas e exócrino", "Vipoma",

      "Adenoma de duto biliar", "Colangiocarcinoma", "Cistadenoma de dutos biliares", "Cistadenocarcinoma de dutos biliares", "Tumor de Klatskin",
      "Adenoma de células hepáticas", "Carcinoma hepatocelular SOE", "Carcinoma hepatocelular fibrolamelar", "Carcinoma hepatocelular e colangiocarcinoma combinados",
      "Adenoma trabecular", "Adenocarcinoma trabecular", "Adenoma embrionário", "Cilindroma dérmico écrino", "Carcinoma cístico adenóide", "Carcinoma cribriforme",
      "Adenoma microcístico", "Pólipo adenomatoso SOE", "Adenocarcinoma in situ em pólipo adenomatoso", "Adenocarcinoma em pólipo adenomatoso",
      "Adenoma tubular SOE", "Adenocarcinoma tubular", "Polipose adenomatosa do cólon", "Adenocarcinoma em polipose adenomatosa do cólon", "Pólipos adenomatosos múltiplos",
      "Adenocarcinoma em pólipos adenomatosos múltiplos", "Carcinoma sólido SOE", "Carcinoma simples", "Tumor carcinóide SOE da apêndice",
      "Tumor carcinóide SOE (exceto do apêndice M8240/1)", "Tumor carcinóide argentafínico SOE", "Tumor carcinóide argentafínico maligno",
      "Carcinoma de células caliciformes", "Carcinóide composto", "Tumor adenocarcinóide", "Carcinoma neuroendócrino", "Carcinoma de células de Merkel", "Apudoma",
      "Adenomatose pulmonar", "Adenocarcinoma bronquíolo-alveolar", "Adenoma alveolar", "Adenocarcinoma alveolar", "Adenoma papilar SOE", "Adenocarcinoma papilar SOE",
      "Adenoma viloso SOE", "Adenocarcinoma in situ em adenoma viloso", "Adenocarcinoma em adenoma viloso", "Adenocarcinoma viloso", "Adenoma tubuloviloso SOE",
      "Adenocarcinoma in situ em adenoma tubuloviloso", "Adenocarcinoma em adenoma tubuloviloso", "Adenoma cromófobo", "Carcinoma cromófobo",

      "Prolactinoma", "Adenoma acidófilo", "Carcinoma acidófilo", "Adenoma misto acidófilo-basófilo", "Carcinoma misto acidófilo-basófilo", "Adenoma oxifílico",
      "Adenocarcinoma oxifílico", "Adenoma basófilo", "Carcinoma basófilo", "Adenoma de células claras", "Adenocarcinoma de células claras SOE", "Tumor hipernefróide",
      "Carcinoma de células renais", "Adenofibroma de células claras", "Carcinoma rico em lípides", "Carcinoma rico em glicogênio", "Carcinoma de células granulares",
      "Adenoma de células principais", "Adenoma de células claras (water-clear cell)", "Adenocarcinoma de células claras (water-clear cell)", "Adenoma de células mistas",
      "Adenocarcinoma de células mistas", "Lipoadenoma", "Adenoma folicular", "Adenocarcinoma folicular SOE", "Adenocarcinoma folicular bem diferenciado",
      "Adenocarcinoma folicular trabecular", "Adenoma microfolicular", "Adenoma macrofolicular", "Carcinoma papilar, variante folicular", "Carcinoma esclerosante não-encapsulado",
      "Adenomas endócrinos múltiplos", "Tumor justaglomerular", "Adenoma de córtex supra-renal SOE", "Carcinoma de córtex supra-renal", "Adenoma de córtex supra-renal de células compactas",
      "Adenoma de córtex supra-renal, variante densamente pigmentada", "Adenoma de córtex supra-renal, de células claras", "Adenoma de córtex supra-renal, de células glomerulosas",
      "Adenoma de córtex supra-renal, de células mistas", "Adenoma endometrióide SOE", "Adenoma endometrióide 'borderline'", "Carcinoma endometrióide",
      "Adenofibroma endometrióide SOE", "Adenofibroma endometrióide 'borderline'", "Adenofibroma endometróide maligno", "Adenoma de apêndice cutâneo", "Carcinoma de apêndice cutâneo",
      "Adenoma de glândula sudorípara", "Tumor de glândula sudorípara SOE",

      "Adenocarcinoma de glândula sudorípara", "Adenoma apócrino", "Adenocarcinoma apócrino", "Acrospiroma écrino", "Espiradenoma écrino", "Hidrocistoma",
      "Hidradenoma papilar", "Siringadenoma papilar", "Siringoma SOE", "Adenoma papilar écrino", "Adenoma sebáceo", "Adenocarcinoma sebáceo", "Adenoma ceruminoso",
      "Adenocarcinoma ceruminoso", "Tumor mucoepidermóide", "Carcinoma mucoepidermóide", "Cistadenoma SOE", "Cistadenocarcinoma SOE", "Cistadenoma seroso SOE",
      "Cistadenocarcinoma seroso SOE", "Cistadenoma seroso 'borderline'", "Cistadenoma papilar SOE", "Cistadenocarcinoma papilar SOE", "Cistadenoma papilar 'borderline'",
      "Tumor cístico papilar", "Cistadenoma seroso papilar SOE", "Cistadenocarcinoma seroso papilar", "Papiloma seroso superficial", "Carcinoma papilar seroso superficial",
      "Cistadenoma seroso papilar 'borderline'", "Cistadenoma mucinoso SOE", "Cistadenocarcinoma mucinoso", "Cistadenoma mucinoso papilar SOE", "Cistadenocarcinoma mucinoso papilar",
      "Cistadenoma mucinoso 'borderline'", "Cistadenoma mucinoso papilar 'borderline'", "Adenoma mucinoso", "Adenocarcinoma mucinoso", "Pseudomixoma de peritônio",
      "Adenocarcinoma produtor de mucina", "Carcinoma de células em anel de sinete", "Carcinoma metastático de células em anel de sinete", "Carcinoma intraductal não infiltrante SOE",
      "Carcinoma de dutos infiltrante", "Comedocarcinoma não infiltrante", "Comedocarcinoma SOE", "Carcinoma juvenil da mama", "Papiloma intraductal",
      "Adenocarcinoma papilar intraductal não infiltrante", "Adenocarcinoma papilar intraductal com invasão", "Adenoma papilar intracístico", "Carcinoma intracístico não infiltrante",

      "Carcinoma intracístico SOE", "Papilomatose intraductal SOE", "Adenoma do mamilo", "Carcinoma medular SOE", "Carcinoma medular com estroma amilóide",
      "Carcinoma medular com estroma linfóide", "Carcinoma lobular in situ", "Carcinoma lobular SOE", "Carcinoma ductular infiltrante", "Carcinoma intraductel e carcinoma lobular in situ",
      "Carcinoma infiltrante de dutos e lobular", "Carcinoma inflamatório", "Doença mamária de Paget", "Doença de Paget e carcinoma de dutos infiltrante da mama",
      "Doença de Paget extramamária (exceto doença de Paget do osso)", "Doença de Paget e carcinoma intraductal da mama", "Adenoma de células acinares",
      "Tumor de células acinares", "Carcinoma de células acinares", "Carcinoma adenoescamoso", "Adenolinfoma", "Carcinoma epitelial-mioepitelial",
      "Adenocarcinoma com metaplasia escamosa", "Adenocarcinoma com metaplasia cartilaginosa e óssea", "Adenocarcinoma com metaplasia de células fusiformes",
      "Adenocarcinoma com metaplasia apócrina", "Timoma benigno", "Timoma maligno", "Tumor dos cordões sexuais e estroma", "Tecoma SOE", "Tecoma maligno",
      "Tecoma luteinizado", "Tumor esclerosante do estroma", "Luteoma SOE", "Tumor de células da granulosa SOE", "Tumor maligno de células da granulosa",
      "Tumor de células da granulosa e células da teca", "Tumor juvenil de células da granulosa", "Tumor dos cordões sexuais com túbulos anulares",
      "Androblastoma benigno", "Androblastoma SOE", "Androblastoma maligno", "Tumor de células de Sertoli-Leydig", "Ginandroblastoma", "Tumor de células de Sertoli SOE",

      "Carcinoma de células de Sertoli", "Tumor de células de Sertoli com depósito de lípides", "Tumor benigno de células de Leydig", "Tumor de células de Leydig SOE",
      "Tumor maligno de células de Leydig", "Tumor de células do hilo", "Tumor de células lipídicas do ovário", "Tumor de supra-renal acessório [resto adrenal]",
      "Paraganglioma SOE", "Paraganglioma maligno", "Paraganglioma simpático", "Paraganglioma parassimpático", "Paraganglioma gangliocítico", "Tumor do glomo jugular",
      "Tumor do corpo aórtico", "Tumor do corpo carotídeo", "Paraganglioma extra-supra-renal SOE", "Paraganglioma extra-supra-renal maligno", "Feocromocitoma SOE",
      "Feocromocitoma maligno", "Glomangiossarcoma", "Tumor de glomo", "Glomangioma", "Glomangiomioma", "Nevo pigmentado SOE", "Melanoma in situ",
      "Melanoma maligno SOE", "Melanoma nodular", "Nevo de células baloniformes", "Melanoma de células baloniformes", "Halo nevo", "Melanoma maligno em regressão",
      "Pápula fibrosa do nariz", "Neuronevo", "Nevo magnocelular", "Nevo displásico", "Nevo não pigmentado", "Melanoma amelanótico", "Nevo juncional SOE",
      "Melanoma maligno em nevo juncional", "Melanose pré-cancerosa SOE", "Melanoma maligno em melanose pré-cancerosa", "Sarda melanótica de Hutchinson SOE",
      "Melanoma maligno em sarda melanótica de Hutchinson", "Melanoma de propagação superficial", "Melanoma lentiginoso maligno das extremidades periféricas",
      "Melanoma desmoplástico maligno", "Nevo intradérmico", "Nevo composto", "Nevo pigmentado gigante SOE", "Melanoma maligno em nevo pigmentado gigante",
      "Nevo epitelióide e de células fusiformes", "Melanoma misto epitelióide e de células fusiformes",

      "Nevo de células epitelióides", "Melanoma de células epitelióides", "Nevo de células fusiformes", "Melanoma de células fusiformes SOE", "Melanoma de células fusiformes, tipo A",
      "Melanoma de células fusiformes, tipo B", "Nevo azul SOE", "Nevo azul maligno", "Nevo azul celular", "Tumor benigno de tecidos moles", "Sarcoma SOE",
      "Sarcomatose SOE", "Sarcoma de células fusiformes", "Sarcoma de células gigantes (exceto de osso M9250/3)", "Sarcoma de células pequenas", "Sarcoma epitelióide",
      "Fibroma SOE", "Fibrossarcoma SOE", "Fibromixoma SOE", "Firbromixossarcoma", "Fibroma periostal", "Fibrossarcoma periostal", "Fibroma de fáscia", "Fibrossarcoma de fáscia",
      "Fibrossarcoma infantil", "Elastofibroma", "Fibromatose agressiva", "Fibromatose abdominal", "Fibroma desmoplástico", "Miofibromatose", "Histiocitoma fibroso SOE",
      "Histiocitoma fibroso atípico", "Histiocitoma fibroso maligno", "Dermatofibroma SOE", "Dermatofibrossarcoma SOE", "Dermatofibrossarcoma protuberante pigmentado",
      "Mixoma SOE", "Mixossarcoma", "Angiomixoma", "Lipoma SOE", "Lipossarcoma SOE", "Fibrolipoma", "Lipossarcoma bem diferenciado", "Fibromixolipoma",
      "Lipossarcoma mixóide", "Lipossarcoma de células redondas", "Lipoma pleomórfico", "Lipossarcoma pleomórfico", "Lipossarcoma misto", "Lipoma intramuscular",
      "Lipoma de células fusiformes", "Lipossarcoma desdiferenciado", "Angiomiolipoma", "Angiolipoma SOE", "Mielolipoma", "Hibernoma", "Lipoblastomatose",

      "Leiomioma SOE", "Leiomiomatose SOE", "Leiomiossarcoma SOE", "Leiomioma epitelióide", "Leiomioma celular", "Leiomioma bizarro", "Angiomioma", "Angiomiossarcoma",
      "Mioma", "Miossarcoma", "Leiomiossarcoma mixóide", "Tumor de músculo liso SOE", "Rabdomioma SOE", "Rabdomiossarcoma SOE", "Rabdomiossarcoma pleomórfico",
      "Rabdomiossarcoma tipo misto", "Rabdomioma fetal", "Rabdomioma adulto", "Rabdomiossarcoma embrionário", "Rabdomiossarcoma alveolar", "Nódulo do estroma endometrial",
      "Sarcoma do estroma endometrial", "Miose endolinfático do estroma", "Adenomioma", "Adenossarcoma", "Adenoma pleomórfico", "Tumor misto maligno SOE",
      "Carcinoma em adenoma pleomórfico", "Tumor mulleriano misto", "Tumor mesodérmico misto", "Nefroma mesoblástico", "Nefroblastoma SOE", "Sarcoma rabdóide",
      "Sarcoma de células claras do rim", "Hepatoblastoma", "Pancreatoblastoma", "Blastoma pulmonar", "Carcinossarcoma SOE", "Carcinossarcoma embrionário",
      "Mioepitelioma", "Mesenquimoma benigno", "Mesenquimoma SOE", "Mesenquimoma maligno", "Sarcoma embrionário", "Tumor de Brenner SOE", "Tumor de Brenner 'borderline'",
      "Tumor de Brenner maligno", "Fibroadenoma SOE", "Fibroadenoma intracanalicular", "Fibroadenoma pericanalicular", "Adenofibroma SOE", "Adenofibroma seroso",
      "Adenofibroma mucinoso", "Fibroadenoma gigante", "Tumor filodes benigno", "Tumor filodes SOE", "Tumor filodes maligno", "Fibroadenoma juvenil", "Sinovioma benigno",
      "Sarcoma sinovial SOE", "Sarcoma sinovial de células fusiformes", "Sarcoma sinovial de células epitelióides", "Sarcoma sinovial bifásico",

      "Sarcoma de células claras (exceto rim M8964/3)", "Mesotelioma benigno", "Mesotelioma maligno", "Mesotelioma fibroso benigno", "Mesotelioma fibroso maligno",
      "Mesotelioma epitelióide benigno", "Mesotelioma epitelióide maligno", "Mesotelioma bifásico benigno", "Mesotelioma bifásico maligno", "Tumor adenomatóide SOE",
      "Mesotelioma cístico", "Disgerminoma", "Seminoma SOE", "Seminoma anaplástico", "Seminoma espermatocítico", "Germinoma", "Carcinoma embrionário SOE",
      "Tumor de seio endodérmico", "Poliembrioma", "Gonadoblastoma", "Teratoma benigno", "Teratoma SOE", "Teratoma maligno SOE", "Teratocarcinoma", "Teratoma maligno não diferenciado",
      "Teratoma maligno intemediário", "Cisto dermóide SOE", "Teratoma com transformação maligna", "Tumor misto de células germinativas", "Estruma ovariana SOE",
      "Estruma ovariana maligna", "Carcinóide estrumal", "Mola hidatiforme SOE", "Mola hidatiforma invasiva", "Coriocarcinoma SOE", "Coriocarcinoma combinado com outros elementos de células germinativas",
      "Teratoma maligno trofoblástico", "Mola hidatiforme parcial", "Tumor trofoblástico do sítio placentário", "Mesomefroma benigno", "Tumor mesonéfrico",
      "Mesomefroma maligno", "Hemangioma SOE", "Hemangiossarcoma", "Hemangioma cavernoso", "Hemangioma venoso", "Hemangioma racemoso", "Sarcoma das células de Kupfer",
      "Hemangioma epitelióde", "Hemangioma histiocitóide", "Hemangioendotelioma benigno", "Hemangioendotelioma SOE", "Hemangioendotelioma maligno", "Hemangioma capilar",
      "Hemangioma intramuscular", "Hemangioendotelioma epitelióide SOE", "Hemangioendotelioma epitelióide maligno", "Tumor alveolar brônquio intravascular", "Sarcoma de Kaposi",

      "Angioqueratoma", "Hemangioma queratótico verrucoso", "Hemangiopericitoma benigno", "Hemangiopericitoma SOE", "Hemangiopericitoma maligno",
      "Angiofibroma SOE", "Hemangioblastoma", "Linfangioma SOE", "Linfangiossarcoma", "Linfangioma capilar", "Linfangioma cavernoso", "Linfangioma cístico",
      "Linfangiomioma", "Linfangiomiomatose", "Hemolinfangioma", "Osteoma SOE", "Osteossarcoma SOE", "Osteossarcoma condroblástico", "Osteossarcoma fibroblástico",
      "Osteossarcoma telangiectásico", "Osteossarcoma em doença de Paget do osso", "Osteossarcoma de células pequenas", "Osteossarcoma justacortical",
      "Osteoma osteóide SOE", "Osteoblastoma SOE", "Osteoblastoma agressivo", "Osteocondroma", "Osteocondromatose SOE", "Condroma SOE", "Condromatose SOE",
      "Condrossarcoma SOE", "Condroma justacortical", "Condrossarcoma justacortical", "Condroblastoma SOE", "Condroblastoma maligno", "Condrossarcoma mixóide",
      "Condrossarcoma mesenquimal", "Fibrossarcoma condromixóide", "Tumor de células gigantes do osso, SOE", "Tumor maligno de células gigantes do osso",
      "Tumor de células gigantes de partes moles, SOE", "Tumor maligno de células gigantes de partes moles", "Sarcoma de Ewing", "Adamantinoma de ossos longos",
      "Fibroma ossificante", "Tumor odontogênico benigno", "Tumor odontogênico SOE", "Tumor odontogênico maligno", "Dentinoma", "Cementoma SOE", "Cementoblastoma benigno",

      "Fibroma cementificante", "Cementoma gigantiforme", "Odontoma SOE", "Odontoma composto", "Odontoma complexo", "Fibro-odontoma ameloblástico",
      "Odontossarcoma ameloblástico", "Tumor odontogênico adenomatóide", "Cisto odontogênico calcificante", "Tumor odontogênico de células fantasma",
      "Ameloblastoma SOE", "Ameloblastoma maligno", "Odontoameloblastoma", "Tumor odontogênico escamoso", "Mixoma odontogênico", "Fibroma odontogênico central",
      "Fibroma odontogênico periférico", "Fibroma ameloblástico", "Fibrossarcoma ameloblástico", "Tumor odontogênico epitelial calcificante", "Craniofaringioma",
      "Pinealoma", "Pineocitoma", "Pineoblastoma", "Tumor neuroectodérmico melanótico", "Tumor neuroectodérmico periférico", "Cordoma", "Glioma maligno",
      "Gliomatose cerebral", "Glioma misto", "Glioma subependimal", "Astrocitoma subependimal de células gigantes", "Papiloma de plexo coróide, SOE",
      "Papiloma maligno de plexo coróide", "Ependimoma SOE", "Ependimoma anaplástico", "Ependimoma papilar", "Ependimoma mixopapilar", "Astrocitoma SOE",
      "Astrocitoma anaplástico", "Astrocitoma protoplásmico", "Astrocitoma gemistocítico", "Astrocitoma fibrilar", "Astrocitoma pilocítico", "Espongioblastoma SOE",
      "Espongioblastoma polar", "Xantoastrocitoma pleomórfico", "Astroblastoma", "Glioblastoma SOE", "Glioblastoma de células gigantes", "Gliossarcoma",
      "Espongioblastoma polar primitivo", "Oligodendroglioma SOE", "Oligodendroglioma anaplástico", "Oligodendroblastoma", "Meduloblastoma SOE",

      "Meduloblastoma desmoplástico", "Medulomioblastoma", "Tumor neuroectodérmico primitivo", "Sarcoma cerebelar SOE", "Sarcoma monstrocelular",
      "Ganglioneuroma", "Ganglioneuroblastoma", "Ganglioneuromatose", "Neuroblastoma SOE", "Meduloepitelioma SOE", "Meduloepitelioma teratóide",
      "Neuroepitelioma SOE", "Espongioneuroblastoma", "Ganglioglioma", "Neurocitoma", "Tumor paciniano", "Retinoblastoma SOE", "Retinoblastoma diferenciado",
      "Retinoblastoma não-diferenciado", "Tumor neurogênico olfatório", "Estesioneurocitoma", "Estesioneuroblastoma", "Estesioneuroepitelioma",
      "Meningioma SOE", "Meningiomatose SOE", "Meningioma maligno", "Meningioma meningoteliomatoso", "Meningioma fibroso", "Meningioma psamomatoso",
      "Meningioma angiomatoso", "Meningioma hemangioblástico", "Meningioma hemangiopericítico", "Meningioma transicional", "Meningioma papilar",
      "Sarcomatose meníngea", "Neurofibroma SOE", "Neurofibromatose SOE", "Neurofibrossarcoma", "Neurofibroma melanótico", "Neurofibroma plexiforme",
      "Neurilemoma SOE", "Neurinomatose", "Neurilemoma maligno", "Tumor tritão maligno", "Neurotequeoma", "Neuroma SOE", "Tumores de células granulares SOE",
      "Tumor maligno de células granulares", "Sarcoma alveolar de partes moles", "Linfoma maligno SOE", "Linfoma maligno não-Hodgkin SOE", "Linfossarcoma SOE",
      "Reticulossarcoma SOE", "Microglioma", "Linfoma maligno difuso SOE", "Doença de Hodgkin SOE", "Doença de Hodgkin de celularidade mista SOE",
      "Doença de Hodgkin, de depleção linfocítica, SOE", "Doença de Hodgkin, de depleção linfocítica, com fibrose difusa", "Doença de Hodgkin, de depleção linfocítica, reticular",

      "Doença de Hodgkin, com predominância linfocítica, SOE", "Doença de Hodgkin, com predominância linfocítica, difusa", "Doença de Hodgkin, com predominância linfocítica, nodular",
      "Paragranuloma de Hodgkin SOE", "Granuloma de Hodgkin", "Sarcoma de Hodgkin", "Doença de Hodgkin, esclerose nodular, SOE", "Doença de Hodgkin, esclerose nodular, fase celular",
      "Doença de Hodgkin, esclerose nodular, predominância linfocítica", "Doença de Hodgkin, esclerose nodular, celularidade mista", "Doença de Hodgkin, esclerose nodular, depleção linfocítica",
      "Linfoma maligno, linfócitos pequenos, SOE", "Linfoma maligno, linfoplasmocítico", "Linfoma maligno de pequenas células clivadas, difuso",
      "Linfoma maligna linfocítico, diferenciação intermediária, difuso", "Linfoma maligno centrocítico", "Linfoma maligno, misto de células pequenas e grandes, difuso",
      "Linfoma maligno, centroblástico-centrocítico, difuso", "Polipose linfomatosa maligna", "Linfoma maligno de células grandes, difuso, SOE",
      "Linfoma maligno, de células grandes, clivadas, difuso", "Linfoma maligno, de células grandes, não clivadas, difuso", "Linfoma maligno, centroblástico, difuso",
      "Linfoma maligno imunoblástico SOE", "Linfoma maligno linfoblástico", "Linfoma maligno, de células pequenas não clivadas, difuso", "Linfoma de Burkitt SOE",
      "Linfoma maligno folicular SOE", "Linfoma maligno, misto de células pequenas clivadas e células grandes, folicular", "Linfoma maligno, centroblástico-centrocítico, folicular",
      "Linfoma maligno, linfocítico, bem diferenciado, nodular", "Linfoma maligno, linfocítico, diferenciação intermediária, nodular", "Linfoma maligno de células pequenas clivadas, folicular",
      "Linfoma maligno linfocítico, pouco diferenciado, nodular", "Linfoma maligno centroblástico folicular", "Linfoma maligno de células grandes, folicular, SOE",
      "Micose fungóide", "Doença de Sézary", "Linfoma de células T periférico SOE", "Linfoma de zona T", "Linfoma linfoepitelóide",

      "Linfoma de células T periféricos (linfadenopatia angio-imunoblástica com disproteinemia)", "Linfoma de células T, periférico, de células pequenas pleomórficas",
      "Linfoma de células T, periférico, pleomórfico de células médias e grandes", "Linfoma cutâneo", "Linfoma de células B monocitóides", "Angioendoteliomatose",
      "Linfoma de células T angiocêntrico", "Linfoma de células grandes (Ki-1+)", "Histiocitose maligna", "Doença de Letterer-Siwe", "Linfoma histiocítico verdadeiro",
      "Plasmocitoma SOE", "Mieloma múltiplo", "Mastocitoma SOE", "Sarcoma de mastócitos", "Mastocitose maligna", "Doença imunoproliferativa SOE",
      "Macroglobulinemia de Waldenström", "Doença da cadeia pesada alfa", "Doença da cadeia pesada gama", "Doença imunoproliferativa do intestino delgado",
      "Gamopatia monoclonal", "Lesão imunoproliferativa angiocêntrica", "Linfadenopatia angioimunoblástica", "Doença linfoproliferativa T-gama", "Leucemia SOE",
      "Leucemia aguda SOE", "Leucemia subaguda SOE", "Leucemia crônica SOE", "Leucemia aleucêmica SOE", "Leucemia linfóide SOE", "Leucemia linfoblástica aguda SOE",
      "Leucemia linfóide subaguda", "Leucemia linfocítica crônica", "Leucemia linfóide aleucêmica", "Leucemia prolinfocítica", "Leucemia de células de Burkitt",
      "Leucemia /linfoma de células T adultas", "Leucemia de plasmócitos", "Eritroleucemia", "Eritremia aguda", "Eritremia crônica", "Leucemia de células de linfossarcoma",
      "Leucemia mielóide SOE", "Leucemia mielóide aguda", "Leucemia mielóide subaguda", "Leucemia mielóide crônica", "Leucemia mielóide aleucêmica",
      "Leucemia promielocítica aguda", "Leucemia mielomonocítica aguda", "Leucemia mielomonocítica crônica", "Leucemia basófila", "Leucemia eosinofílica",

      "Leucemia monocítica SOE", "Leucemia monocítica aguda", "Leucemia monocítica subaguda", "Leucemia monocítica crônica", "Leucemia monocítica aleucêmica",
      "Leucemia de mastócitos", "Leucemia megacarioblástica aguda", "Sarcoma mielóide", "Panmielose aguda", "Mielofibrose aguda", "Leucemia 'hairy cell'",
      "Reticuloendoteliose leucêmica", "Policitemia vera", "Doença mieloproliferativa crônica", "Mieloesclerose com metaplasia mielóide", "Trombocitemia idiopática",
      "Doença linfoproliferativa SOE", "Anemia refratária SOE", "Anemia refratária sem sideroblastos", "Anemia refratária com sideroblastos", "Anemia refratária com excesso de blastos",
      "Anemia refratária com excesso de blastos com transformação", "Síndrome mielodisplásica SOE",
      " Neoplasia maligna, incerta se primária ou metastática​",	" Tumor maligno de células claras​",	" Carcinomatose​",	" Carcinoma neuroendócrino de grandes células​",	" Carcinoma de grandes células, fenótipo rabdóide​",	" Carcinoma de células 'vítreas'​",	" Carcinoma de células gigantes tipo osteoclasto​",	" Carcinoma de células não pequenas​",	" Carcinoma escamoso, papilar não invasivo​",	" Carcinoma de células escamosas corneificadas​",	" Carcinoma escamocelular, basalóide​",	" Carcinoma escamoso, tipo células claras​",	" Carcinoma basocelular nodular​",	" Carcinoma basocelular adenóide​",	" Tumor maligno de células granulares​",	" Neoplasia benigna do epitélio glandular​",	" Carcinoma in situ do epitélio glandular​",	" Carcinoma de células transicionais, micropapilífero​",	" Neoplasia glandular intraepitelial, grau III​",	" Adenoma de glândulas intestinais​",	" Somatostatinoma, SOE​",	" Carcinoma hepatocelular, tipo esclerosante​",	" Carcinoma hepatocelular, variante fusocelular​",	" Carcinoma hepatocelular, tipo células claras​",	" Carcinoma hepatocelular, tipo pleomórfico​(CIDZERO)",	" Carcinoma ductal in situ tipo cribriforme (C50.-)​",	" Adenocarcinoma de células parietais (C16.-)​",	" Adenocarcinoma de glândulas anais (C21.1)​",	" Carcinoma ductal in situ tipo sólido (C50.-)​",	" Carcinóide de células semelhantes a enterocromafinicas, SOE​",	" Tumor maligno de células semelhantes a enterocromafinicas​",	" Carcinóide tubular​",	" Tumor carcinoide atípico​",	" Carcinoma bronquíolo-alveolar não mucinoso (C34.-)​",	" Carcinoma bronquíolo-alveolar mucinoso (C34.-)​",	" Carcinoma bronquíolo-alveolar misto, mucinoso e não mucinoso (C34.-)​(CIDZERO_R)",	" Adenocarcinoma com subtipos mistos​",	" Carcinoma pituitário, SOE (C75.1)​",	" Adenocarcinoma de células claras (C56.9)​",	" Carcinoma de células renais associado a cisto (C64.9)​",	" Carcinoma renal, cromófobo (C64.9)​",	" Carcinoma renal, sarcomatoide (C64.9)​",	" Carcinoma de ductos coletores (C64.9)​",	" Adenoma folicular atípico (C73.9)​",	" Adenocarcinoma fetal (C73.9)​",	" Carcinoma folicular encapsulado (C73.9)​",	" Carcinoma insular (C73.9)​",	" Microcarcinoma papilífero (C73.9)​",	" Carcinoma papilífero, tipo células oxifílicas (C73.9)​",	" Carcinoma papilífero encapsulado (C73.9)​",	" Carcinoma papilífero, células colunares (C73.9)​",	" Carcinoma papilífero, variante de células altas (C73.9)​",	" Carcinoma misto medular e folicular (C73.9)​",	" Carcinoma misto medular papilífero (C73.9)​",	" Adenocarcinoma endometrioide, tipo secretor​",	" Adenocarcinoma endometrioide, tipo células ciliadas​",	" Adenocarcinoma tipo endocervical​",	" Tumor de Brenner maligno (C56.9)​(CIDZERO)",	" Espiradenoma écrino maligno",	" Carcinoma ductal de glândula sudorípara, esclerosante",	" Adenocarcinoma écrino papilífero",	" Poroma ecrino maligno",	" Adenocarcinoma écrino",	" Tumor seroso atípico proliferativo",	" Tumor cístico de células claras de malignidade limítrofe",	" Adenoma viloso",	" Carcinoma sólido pseudopapilífero",	" Tumor intraductal papilífero mucinoso com displasia moderada",	" Carcinoma intraductal papilífero mucinoso, não invasivo",	" Carcinoma intraductal papilífero mucinoso, invasivo",	" Tumor cístico papilífero seroso de malignidade limítrofe",	" Tumor papilífero seroso de malignidade limítrofe",	" Cistadenocarcinoma mucinoso, não invasivo",	" Tumor cístico mucinoso de malignidade limítrofe",	" Adenoma seroso de células claras, borderline",	" Adenocarcinoma mucinoso, tipo endocervical",	" Carcinoma intraductal micropapilífero",	" Carcinoma cístico hipersecretor",	" Carcinoma medular atípico",	" Carcinoma ductal, tipo desmoplásico",	" Carcinoma misto (ducto infiltrativo e de outros tipos)",
      "Carcinoma lobular infiltrativo misto com outros tipos de carcinoma",	" Adenocarcinoma polimorfo de baixo grau",	" Carcinoma tubular",	" Cistoadenocarcinoma tipo células acinares",	" Adenocarcinoma com diferenciação neuroendócrina",	" Carcinoma metaplásico, SOE",	" Adenocarcinoma hepatoide",	" Timoma, tipo A, SOE",	" Timoma, tipo A, SOE",	" Timoma, Tipo A, maligno",	" Timoma, tipo AB, SOE",	" Timoma maligno tipo AB",	" Timoma, tipo B1, SOE",	" Timoma maligno tipo B1",	" Timoma tipo B2, SOE",	" Timoma maligno tipo B2",	" Timoma tipo B3, SOE",	" Timoma maligno tipo B3",	" Timoma tipo C",	" Tumor epitelial tipo fusiforme com elementos semelhantes ao timo",	" Carcinoma semelhante a células tímicas",	" Tumor do estroma dos cordões sexuais, parcialmente diferenciado.",	" Tumor do estromal dos cordões sexuais, formas mistas",	" Tumor estromal com poucos elementos dos cordões sexuais",	" Tumor de células de Sertoli-Leydig, de diferenciação intermediária",	" Tumor de células de Sertoli-Leydig, pouco diferenciado",	" Tumor de células de Sertoli-Leydig, retiforme",	" Tumor de células de Sertoli-Leydig, pouco diferenciado, com elementos heterólogos",	" Tumor de células de Sertoli, células grandes calcificadas",	" Tumor de células de Sertoli-Leydig, pouco diferenciado",	" Tumor maligno de células produtoras de esteroides",	" Tumor glômico maligno",	" Melanoma meníngeo difuso",	" Neoplasia melanocítica intraepitelial",	" Melanoma lentiginoso acral",	" Tumor mesenquimal indiferenciado",	" Sarcoma indiferenciado",	" Tumor desmoplásico de pequenas células redondas",	" Fibroma de células grandes",	" Tumor fibroso solitário maligno",	" Tumor miofibroblástico inflamatório",	" Tumor miofibroblástico maligno",	" Tumor fibrohistiocítico plexiforme",	" Histiocitoma fibroso angiomatóide",	" Lipoma atípico",	" Tumor de malignidade indeterminada",	" Leiomiossarcoma",	" Leiomioma metastatizante",	" Rabdomiossarcoma fusocelular",	" Rabdomiossarcoma alveolar",	" Sarcoma do estroma endometrial de baixo grau",	" Carcinofibroma",	" Tumor estromal gastrointestinal, benigno",	" Tumor estromal gastrointestinal maligno",	" Tumor estromal gastrointestinal, malignidade incerta",	" Tumor estromal gastrointestinal maligno",	" Nefroblastoma cístico parcialmente diferenciado",	" Nefroma cístico maligno",	" Blastoma pleuropulmonar",	" Sialoblastoma",	" Mioepitelioma maligno",	" Adenofibroma seroso de malignidade limítrofe",	" Adenocarcinofibroma seroso",	" Adenofibroma mucinoso de malignidade limítrofe",	" Adenocarcinofibroma mucinoso",	" Neoplasia maligna intratubular de células germinativas",	" Tumor de células germinativas não seminomatoso",	" Tumor trofoblástico epitelióide",	" Osteossarcoma central",	" Osteossarcoma central bem diferenciado",	" Osteossarcoma telangiectásico",	" Osteossarcoma perióstal",	" Osteossarcoma de superfície, alto grau",	" Condrossarcoma de células claras",	" Condrossarcoma desdiferenciado",	" Tumor tenossinovial de células gigantes maligno",	" Craniofaringioma, tipo adamantinoma",	" Craniofaringioma papilífero",	" Tumor de Askin",	" Cordoma",	" Cordoma condróide",	" Cordoma desdiferenciado",	" Papiloma do plexo coróide atípico",	" Ependimoma anaplásico",	" Astrocitoma infantil desmoplásico",	" Tumor neuroepitelial disembrioplástico",	" Astrocitoma pilocítico",	" Glioblastoma multiforme",	" Gliofibroma",	" Glioma cordóide",	" Meduloblastoma de células grandes",	" Ganglioglioma anaplásico",	" Neurocitoma central",	" Tumor teratoide/rabdóide atípico",	" Retinoblastoma difuso",	" Meningioma rabdóide",	" Meningioma atípico",	" Perineuroma maligno",	" Linfoma composto, Hodgkin/não Hodgkin",	" Linfoma de Hodgkin, rico em linfócitos",	" Linfoma mediastinal primário de grandes células B",
      " Linfoma da zona marginal esplênica",	" Linfoma da zona marginal tipo células B, SOE",	" Linfoma MALT",	" Linfoma subcutâneo, tipo paniculite de células T",	" Linfoma hepatoesplênico tipo gama-delta",	" Linfoma intestinal de células T",	"Lesão linfoproliferativa cutânea primária de célula T (CD30+) (C44.-)",	" Linfoma nasal e tipo nasal de células T/NK",	" Linfoma linfoblástico de células precursoras, SOE",	" Linfoma linfoblástico de células precursoras B",	" Linfoma linfoblástico de células precursoras T",	"Leucemia de plasmocitos (C42.1)",	"Plamocitoma extramedular",	" Leucemia células mastocitárias",	"Histiocitose maligna",	" Histiocitose de células de Langerhans, SOE",	" Histiocitose de células de Langerhans, multifocal",	" Histiocitose de células de Langerhans unifocal",	" Histiocitose de células de Langerhans, poliostótica",	" Sarcoma histiocítico",	"Sarcoma histiocitico",	" Sarcoma de células de Langerhans",	" Sarcoma de células dendríticas interdigitantes",	" Sarcoma de células dendríticas foliculares",	" Doença de depósito de imunoglobulina",	" Leucemia aguda, bifenotípica",	" Leucemia linfocítica granular de células grandes tipo T",	" Leucemia prolinfocítica tipo célula B",	" Leucemia prolinfocítica tipo célula T",	" Leucemia linfoblástica de células precursoras, SOE",	" Leucemia linfoblástica de células precursoras tipo B",	" Leucemia linfoblástica de células precursoras tipo T",	"Leucemia linfoblástica de células T precursora",	" Leucemia mieloide aguda com eosinófilos anormais na medula",	" Leucemia mieloide aguda, com diferenciação mínima",	" Leucemia mieloide aguda sem maturação",	" Leucemia mieloide aguda com maturação",	" Leucemia crônica mielogênica, BCR/ABL positiva",	" Leucemia mieloide crônica atípica, BCR/ABL negativa",	" Leucemia mieloide aguda com displasia com multilinhagem",	" Leucemia mieloide aguda, t(8;21)",	" Leucemia mieloide aguda com anomalias em 11q23",	" Leucemia mieloide aguda relacionada ao tratamento",	" Leucemia mielomonocítica crônica",	" Leucemia mielomonocítica juvenil",	" Leucemia agressiva de células NK",	" Policitemia vera",	" Síndrome hipereosinofílica",	"Trombocitemia essencial",	" Citopenia refratária com displasia multilinhagem",	" Síndrome mielodisplásica relacionada ao tratamento",	"Doença mieloproliferativa crônica classificavel",	" Anemia refratária com sideroblastos em anel",	" RAEB I (Refratária Anemia com Excesso de Blastos)",	" RAEB II (Refratária Anemia com Excesso de Blastos)",	" RAEB-T (Refratária Anemia com Excesso de Blastos em Transformação)",	"Citopenia refratária com displasia multilinear (síndrome mielodisplásica)",	" Síndrome mielodisplásica com deleção 5q (5q-)",	"Síndrome mielodisplásica relacionada a terapia, SOE",	"Síndrome mielodisplásica, SOE",	"clinicamente tumor maligno (câncer)",
      "Neoplasia benigna",	"Tumor de células claras, SOE",	"Glucagonoma, SOE (C25.-)",	"Somatostatinoma maligno",	"Adenoma pituitario, SOE (C75.1)",	"Adenofibroma de células claras, malignidade limítrofe (C56.9)",	"Adenoma papilar digitiforme agressivo (C44.-)",	"Tumor de células de Sertoli-Leydig com diferenciação intermediaria e elementos heterologos",	"Melanocitoma meningeano (C70.9)",	"Lesão proliferativa dérmica em nevos congênitos (C44.-)",	"Miofibroblastoma",	"Tumor miofibroblástico peribrônquico (C34.-)",	"Fibroblastoma de células gigantes",	"Lipossarcoma fibroblástico",	"Tumor estromal gastrointestinal benigno",	"Angioendotelioma papilar endovascular",	"Hemangioendotelioma fusocelular",	"Osteossarcoma intra-cortical (C40.-, C41.-)",	"Carcinossarcoma odontogênico",	"Ganglioneuromatose",	"Perineuroma, SOE",	"Linfoma associado a tecido linfóide mucoso",	"Mieloesclerose com metaplasia mielóide",	"Clinicamente tumor, SOE",	"Base do diagnóstico não mensionado"


    )
  )

  # Verifica se a coluna Tipo_Histologico existe antes de tentar acessá-la
  if ("Tipo_Histologico" %in% names(dados)) {
    message("Mapeando os códigos de Tipo Histológico para nomes completos.")
    # Mapeamento dos códigos de Tipo Histológico para nomes usando um vetor nomeado
    map <- setNames(tipo_histologico_names$Tipo_Histologico_Completo, tipo_histologico_names$Codigo_tipo_hist)
    dados$Tipo_Histologico_Completo <- map[as.character(dados$Tipo_Histologico)]
    message("Nomes completos dos Tipos Histológicos adicionados.")
  } else {
    stop("\033[1;31mA coluna 'Tipo_Histologico' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message(paste("\033[1;32m", "> Ajuste dos códigos de Tipo Histológico concluído com sucesso. Foi adicionada uma coluna no dataframe, chamada Tipo_Histologico_Completo.", "\033[0m"))

  return(dados)

}

