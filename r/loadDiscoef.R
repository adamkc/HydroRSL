loadDiscoef <- function(){
  discoef <<- structure(list(n = c(0, 39, 53, 47, 0, 46, 0, 0, 55,
                                   0, 0, 0, 0, 0, 70, 0, 0, 0, 0, 0, 0, 0), 
                             a = c(0, 0.5313, 0.2547, 0.3101, 0, 0.2663, 0,
                                   0, 0.2624, 0, 0, 0, 0, 0, 0.4753, 0, 0,
                                   0, 0, 0, 0, 0), 
                             b = c(1, 0.9268, 0.9592, 0.9645, 1, 0.9819, 1, 1,
                                   0.9424, 1, 1, 1, 1, 1, 0.9009, 1, 1, 1, 1,
                                   1, 1, 1), 
                             max = c(NA, 450, 2000, 2000, NA, 900, NA, NA, 800,
                                     NA, NA, NA, NA, NA, 800, NA, NA, NA, NA,
                                     NA, NA, NA)), 
                        row.names = c("ARF", "CAR", "DOL", "EAG", "FTR", "HEN",
                                      "IVE", "MUN", "NFC", "OGI", "POR", "QUE",
                                      "RIC", "SEQ", "SFC", "TRE", "UQL", "WIL",
                                      "ZIE", "XYZ", "YOC", "XRA"), 
                        class = "data.frame")
}
