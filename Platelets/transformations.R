# attempt  to find appropriate transformations

boxTidwell(
  formula= Platelet_1000.mL ~ Age  + Pulse_rate_PPM + Creatine_mg.dL + Hemoglobin_g.dL + Potassium_mEq.lit + White_Blood_Cells.mL,
  other.x= ~ Sex + Current_Smoker + Atypical + Nonanginal_Chest_Pain,
  data= platelets_NoCAD
)

