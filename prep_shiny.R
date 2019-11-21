library(fs)

file_copy(path = "graphics/trace_OK.gif", new_path = "Gun_Violence_Bay_Area/trace_OK.gif", overwrite = TRUE)

file_copy(path = "graphics/AAGun_SF.gif", new_path = "Gun_Violence_Bay_Area/AAGun_SF.gif", overwrite = TRUE)

file_copy(path = "RDS/graphic_violence.RDS", new_path = "Gun_Violence_Bay_Area/graphic_violence.RDS", overwrite = TRUE)

file_copy(path = "RDS/graphic_violence_capita.RDS", new_path = "Gun_Violence_Bay_Area/graphic_violence_capita.RDS", overwrite = TRUE)

file_copy(path = "RDS/imprisonment.RDS", new_path = "Gun_Violence_Bay_Area/imprisonment.RDS", overwrite = TRUE)

file_copy(path = "RDS/laws.RDS", new_path = "Gun_Violence_Bay_Area/laws.RDS", overwrite = TRUE)
