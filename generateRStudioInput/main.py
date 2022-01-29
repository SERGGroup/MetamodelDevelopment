from generateRStudioInput.xml_generation import convert_excel_to_xml, create_rstudio_xml
from tkinter import filedialog
import tkinter as tk
import xlrd, os


def convert_excel_to_xml_ORC_LCOE_simplified():

    input_dict = {

        "names": [

            "T_in", "x_in",
            "m_brine", "eta_turb"

        ],
        "units": [

            "[C]", "[-]",
            "[kg/s]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "P_eva", "LCOE_plant"

        ],
        "units": [

            "[kPa]", "[€/kWh]"

        ]

    }

    def check_func(input_list: list):

        try:

            return float(input_list[5]) > 0 and int(input_list[5]) < 100

        except:

            return False

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_ORC_LCOE():

    input_dict = {

        "names": [

            "T_in", "x_in",
            "m_brine", "eta_turb"

        ],
        "units": [

            "[C]", "[-]",
            "[kg/s]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "P_eva", "LCOE_plant", "Z_dot_tot", "cost_tot",
            "surf_2", "surf_3", "surf_5", "n_well"

        ],
        "units": [

            "[kPa]", "[€/kWh]", "[€/s]", "[€]",
            "[m^2]", "[m^2]", "[m^2]", "[-]"

        ]

    }

    def check_func(input_list: list):

        try:

            return float(input_list[5]) > 0 and not int(input_list[5]) == 29

        except:

            return False

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_ORC_new():

    input_dict = {

        "names": [

            "T_in", "x_in",
            "m_brine", "eta_turb"

        ],
        "units": [

            "[C]", "[-]",
            "[kg/s]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "P_eva", "W_net", "eta_I", "W_pump",
            "W_turb", "Q_HRE", "Q_HRSG", "Q_COND"

        ],
        "units": [

            "[kPa]", "[kW]", "[-]", "[kW]",
            "[kW]", "[kW]", "[kW]", "[kW]"

        ]

    }

    def check_func(input_list: list):

        try:
            return float(input_list[5]) > 300 and not int(input_list[5]) == 29 and float(input_list[0]) > 230 #and float(input_list[0]) < 220
        except:
            return False

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_ORC_HRE():

    input_dict = {

        "names": [

            "T_in", "x_in",
            "m_brine", "eta_turb"

        ],
        "units": [

            "[C]", "[-]",
            "[kg/s]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "P_eva", "W_net", "eta_I", "W_pump",
            "W_turb", "Q_HRSG", "Q_COND"

        ],
        "units": [

            "[kPa]", "[kW]", "[-]", "[kW]",
            "[kW]", "[kW]", "[kW]"

        ]

    }

    def check_func(input_list: list):

        try:
            return float(input_list[5]) > 0 and not int(input_list[5]) == 29 #and float(input_list[0]) > 160
        except:
            return False

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_ORC():

    input_dict = {

        "names": [

            "T_in", "x_in",
            "m_brine", "eta_turb"

        ],
        "units": [

            "[C]", "[-]",
            "[kg/s]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "W_net", "eta_I", "W_pump",
            "W_turb", "Q_HRE", "Q_HRSG"

        ],
        "units": [

            "[kW]", "[-]", "[kW]",
            "[kW]", "[kW]", "[kW]"

        ]

    }

    def check_func(input_list: list):

        return input_list[8] > 0 and input_list[0] > 150

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_flash():

    input_dict = {

        "names": [

            "T_in", "m_fluid", "x_sep", "eta_turb"

        ],
        "units": [

            "[C]", "[kg/s]", "[-]", "[-]"

        ]

    }
    output_dict = {

        "names": [

            "W_net", "eta_stage", "LCOE", "Cost_tot"

        ],
        "units": [

            "[kW]", "[-]", "[€/kWh]", "[€]"

        ]

    }

    def check_func(input_list: list):

        return True

    return convert_excel_to_xml(input_dict, output_dict, first_row=4, first_col=1, check_func=check_func)


def convert_excel_to_xml_zekeryia():
    input_dict = {

        "names": [

            "P_1", "T_1", "m_1",
            "P_15", "T_15", "m_15"

        ],
        "units": [

            "[kPa]", "[C]", "[kg/s]",
            "[kPa]", "[C]", "[kg/s]"

        ]

    }
    output_dict = {

        "names": [

            "Q_HE", "eta", "UA",
            "DP_1_2", "DP_15_10", "Q_coeff"

        ],
        "units": [

            "[kJ/h]", "[-]", "[kJ/(h C)]",
            "[kPa]", "[kPa]", "[-]"

        ]

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_BHE():
    input_dict = {

        "names": ["mass_flow", "T_in", "P_in", "delta_D_ext"],
        "units": ["[kg/s]", "[C]", "[bar]", "[inches]"]

    }
    output_dict = {

        "names": [

            "Q_BHE", "v_max", "Ex_BHE",
            "DT", "insulation", "DP"

        ],
        "units": [

            "[kW]", "[m/s]", "[kW]",
            "[C]", "[m]", "[bar]"

        ],

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_BHE_check():
    input_dict = {

        "names": ["mass_flow", "T_in", "P_in", "delta_D_ext"],
        "units": ["[kg/s]", "[C]", "[bar]", "[inches]"]

    }
    output_dict = {

        "names": [

            "Q_error", "v_max_error", "Ex_error",
            "DT_error", "DT_eval_error", "insulation_error", "DP_error"

        ],
        "units": [

            "[kW]", "[m/s]", "[kW]",
            "[C]", "[C]", "[m]", "[bar]"

        ]

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_BHE_simple_cycle():
    input_dict = {

        "names": ["P_in", "T_in", "mass_flow"],
        "units": ["[kPa]", "[C]", "[kg/s]"]

    }
    output_dict = {

        "names": [

            "W_turb", "eta_I", "eta_II",
            "DP_BHE", "DT_BHE"

        ],
        "units": [

            "[kW]", "[ - ]", "[ - ]",
            "[bar]", "[C]"

        ]

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_BHE_cycle():
    input_dict = {

        "names": ["P_in", "mass_flow"],
        "units": ["[kPa]", "[kg/s]"]

    }
    output_dict = {

        "names": ["eta_I", "m_co2", "W_turb"],
        "units": ["[ - ]", "[kg/s]", "[kW]"]

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_BHE_overall_calculation():
    input_dict = {

        "names": ["mass_flow", "T_in", "P_in"],
        "units": ["[kg/s]", "[C]", "[MPa]"]

    }
    output_dict = {

        "names": ["Q", "v_max", "Ex", "DT", "ins", "DP", "W_turb", "Q_DH"],
        "units": ["[MW]", "[m/s]", "[MW]", "[C]", "[m]", "[bar]", "[kW]", "[kW]"]

    }

    return convert_excel_to_xml(input_dict, output_dict)


def convert_excel_to_xml_pomarance():
    root = tk.Tk()
    root.withdraw()
    excel_path = filedialog.askopenfilename()
    file = xlrd.open_workbook(excel_path)

    # sheet = file.get_sheet(0)

    input_values_list = [list(), list(), list(), list()]
    output_values_list = [list(), list(), list(),
                          list(), list(), list(),
                          list(), list(), list()]

    for sheet in file.sheets():

        mass_split_ratio = float(sheet.name.strip("%m=").replace(",", "."))
        input_values_list[0].extend([mass_split_ratio] * len(sheet.col_values(0, start_rowx=3)))

        for col in range(3):
            input_values_list[col + 1].extend(sheet.col_values(col, start_rowx=3))

        for col in range(3, len(output_values_list) + 3):
            output_values_list[col - 3].extend(sheet.col_values(col, start_rowx=3))

    input_dict = {"names": ["m_perc", "m_GEO", "T_GEO_in", "T_DH_in"],
                  "units": ["", "kg/s", "C", "C"],
                  "values": input_values_list}

    output_dict = {"names": ["eta", "m_DH", "Q_cond",
                             "Q_DH", "Q_SH", "T_GEO_out",
                             "T_DH_out", "W_HPT", "W_LPT", "W_net"],

                   "units": ["", "kg/s", "kW",
                             "kW", "kW", "C",
                             "C", "kW", "kW", "kW"],

                   "values": output_values_list}

    create_rstudio_xml(os.path.dirname(excel_path), input_dict, output_dict)

    return {"input": input_dict, "output": output_dict}


if __name__ == '__main__':

    return_dict = convert_excel_to_xml_ORC_LCOE_simplified()
    print(return_dict)
