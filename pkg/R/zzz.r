scale_sides <- ScaleSides$build_accessor()
# scale_sides_continuous <- ScaleSidesContinuous$build_accessor() # bug somewhere,  
# use cut(variable, 7) 

scale_angle <- ScaleAngle$build_accessor()
scale_angle_discrete <- ScaleAngleDiscrete$build_accessor()

scale_ar <- ScaleAr$build_accessor() # default is continuous
scale_ar_discrete <- ScaleArDiscrete$build_accessor()

scale_length <- ScaleLength$build_accessor() # default is continuous

geom_ngon <- GeomNgon$build_accessor()
geom_star <- GeomStar$build_accessor()
geom_ellipse <- GeomEllipse$build_accessor()
geom_field <- GeomField$build_accessor()
# geom_custom <- GeomCustom$build_accessor() # not sure how to make this one work


scale_angle_manual <- ScaleManual$build_accessor(list(variable = "\"angle\""))
scale_ar_manual <- ScaleManual$build_accessor(list(variable = "\"ar\""))
scale_sides_manual <- ScaleManual$build_accessor(list(variable = "\"sides\""))
scale_length_manual <- ScaleManual$build_accessor(list(variable = "\"length\""))

scale_alpha_manual <- ScaleManual$build_accessor(list(variable = "\"alpha\""))

scale_colour_dichromat <- ScaleDichromat$build_accessor(list(variable = "\"colour\""))
scale_fill_dichromat <- ScaleDichromat$build_accessor(list(variable = "\"fill\""))
