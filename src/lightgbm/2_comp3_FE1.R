require("data.table")
#require("tidyverse")

### FEATURE ENGINEERING ##########

#Cargo datos
dataset <- fread("buckets/b1/datasets/competencia_03.csv.gz")


# Catastrophe (código 'prestado' de Juan Raman)

dataset[foto_mes == 201901, ctransferencias_recibidas := NA]
dataset[foto_mes == 201901, mtransferencias_recibidas := NA ]
dataset[foto_mes == 201902, ctransferencias_recibidas := NA]
dataset[foto_mes == 201902, mtransferencias_recibidas := NA]
dataset[foto_mes == 201903, ctransferencias_recibidas := NA]
dataset[foto_mes == 201903, mtransferencias_recibidas := NA]
dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, ctransferencias_recibidas := NA]
dataset[foto_mes == 201904, mtransferencias_recibidas := NA]
dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, Visa_mfinanciacion_limite := NA]
dataset[foto_mes == 201905, ccomisiones_otras := NA]
dataset[foto_mes == 201905, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201905, ctransferencias_recibidas := NA]
dataset[foto_mes == 201905, mactivos_margen := NA]
dataset[foto_mes == 201905, mcomisiones := NA]
dataset[foto_mes == 201905, mcomisiones_otras := NA]
dataset[foto_mes == 201905, mpasivos_margen := NA]
dataset[foto_mes == 201905, mrentabilidad_annual := NA]
dataset[foto_mes == 201905, mrentabilidad := NA]
dataset[foto_mes == 201905, mtransferencias_recibidas := NA]
dataset[foto_mes == 201910, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, ccomisiones_otras := NA]
dataset[foto_mes == 201910, chomebanking_transacciones := NA]
dataset[foto_mes == 201910, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 201910, mactivos_margen := NA]
dataset[foto_mes == 201910, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, mcomisiones := NA]
dataset[foto_mes == 201910, mcomisiones_otras := NA]
dataset[foto_mes == 201910, mpasivos_margen := NA]
dataset[foto_mes == 201910, mrentabilidad_annual := NA]
dataset[foto_mes == 201910, mrentabilidad := NA]
dataset[foto_mes == 201910, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202001, cliente_vip := NA]
dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes == 202006, catm_trx := NA]
dataset[foto_mes == 202006, catm_trx_other := NA]
dataset[foto_mes == 202006, ccajas_consultas := NA]
dataset[foto_mes == 202006, ccajas_depositos := NA]
dataset[foto_mes == 202006, ccajas_extracciones := NA]
dataset[foto_mes == 202006, ccajas_otras := NA]
dataset[foto_mes == 202006, ccajas_transacciones := NA]
dataset[foto_mes == 202006, ccallcenter_transacciones := NA]
dataset[foto_mes == 202006, ccheques_depositados := NA]
dataset[foto_mes == 202006, ccheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, ccheques_emitidos := NA]
dataset[foto_mes == 202006, ccheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, ccomisiones_otras := NA]
dataset[foto_mes == 202006, cextraccion_autoservicio := NA]
dataset[foto_mes == 202006, chomebanking_transacciones := NA]
dataset[foto_mes == 202006, cmobile_app_trx := NA]
dataset[foto_mes == 202006, ctarjeta_debito_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_master_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_visa_transacciones := NA]
dataset[foto_mes == 202006, ctrx_quarter := NA]
dataset[foto_mes == 202006, mactivos_margen := NA]
dataset[foto_mes == 202006, matm := NA]
dataset[foto_mes == 202006, matm_other := NA]
dataset[foto_mes == 202006, mautoservicio := NA]
dataset[foto_mes == 202006, mcheques_depositados := NA]
dataset[foto_mes == 202006, mcheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, mcheques_emitidos := NA]
dataset[foto_mes == 202006, mcheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, mcomisiones := NA]
dataset[foto_mes == 202006, mcomisiones_otras := NA]
dataset[foto_mes == 202006, mcuentas_saldo := NA]
dataset[foto_mes == 202006, mextraccion_autoservicio := NA]
dataset[foto_mes == 202006, mpasivos_margen := NA]
dataset[foto_mes == 202006, mrentabilidad_annual := NA]
dataset[foto_mes == 202006, mrentabilidad := NA]
dataset[foto_mes == 202006, mtarjeta_master_consumo := NA]
dataset[foto_mes == 202006, mtarjeta_visa_consumo := NA]
dataset[foto_mes == 202006, tcallcenter := NA]
dataset[foto_mes == 202006, thomebanking := NA]

###### Variables historicas ######

setorder(dataset,cols = foto_mes)

## Lags de variables 1,3 y 6 meses

lagcols <- setdiff(names(dataset),c("numero_de_cliente","foto_mes","clase_ternaria"))


lagcols1_names <- paste0("lag1_",lagcols)
lagcols3_names <- paste0("lag3_",lagcols)
lagcols6_names <- paste0("lag6_", lagcols)


dataset[, (lagcols1_names) :=  shift(.SD, 1), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols3_names) :=  shift(.SD, 3), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols6_names) :=  shift(.SD, 6), .SDcols = lagcols, by=numero_de_cliente]

### Deltas 1, 3 y 6 meses

for (col in lagcols) {
  dataset[, paste0("delta1_" ,col) := get(col) - get(paste0("lag1_",col))]
} 

for (col in lagcols) {
  dataset[, paste0("delta3_" ,col) := get(col) - get(paste0("lag3_",col))]
} 

for (col in lagcols) {
  dataset[, paste0("delta6_" ,col) := get(col) - get(paste0("lag6_",col))]
} 

fwrite(dataset,
       file = "buckets/b1/datasets/competencia_03_fe1.csv.gz",
       sep = ",")
