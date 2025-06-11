library(nnet)

# Normalizar los predictores individualmente
diabetes$Pregnancies = scale(diabetes$Pregnancies)
diabetes$Glucose = scale(diabetes$Glucose)
diabetes$BloodPressure = scale(diabetes$BloodPressure)
diabetes$SkinThickness = scale(diabetes$SkinThickness)
diabetes$Insulin = scale(diabetes$Insulin)
diabetes$BMI = scale(diabetes$BMI)
diabetes$DiabetesPedigreeFunction = scale(diabetes$DiabetesPedigreeFunction)
diabetes$Age = scale(diabetes$Age)

# Crear el modelo de red neuronal
modelo = nnet(Outcome ~ Pregnancies + Glucose + BloodPressure + 
                SkinThickness + Insulin + BMI + 
                DiabetesPedigreeFunction + Age,
              data = diabetes,
              size = 3, maxit = 500, decay = 0.01, linout = FALSE)

print(modelo)

# Hacer predicciones
predicciones = predict(modelo, diabetes, type = "raw")
print(predicciones)

# Convertir probabilidades a clases (0 o 1)
predicciones_clase = ifelse(predicciones > 0.5, 1, 0)

# Comparar con los valores reales
tabla = table(Predicho = predicciones_clase, Real = diabetes$Outcome)
print(tabla)

# Medir precisión
Precision = sum(diag(tabla)) / sum(tabla)
cat("Precisión del modelo:", round(Precision * 100, 2), "%\n")
