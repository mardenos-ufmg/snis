from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity

def Bartlett(dados):
    chi_square_value, p_value = calculate_bartlett_sphericity(dados)
    if (p_value > 0.05):
        return False
    else:
        return True         