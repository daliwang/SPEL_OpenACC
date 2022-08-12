
home_dir = "/home/peter/ELM_OpenACC_test-unit/"
elm_files = home_dir+"SourceFiles/"
unittests_dir = home_dir+"unit-tests/"

default_mods = ['shr_kind_mod.F90', 'elm_varctl.F90', 'elm_varcon.F90', 'shr_const_mod.F90', 'elm_varpar.F90',
                 'decompMod.F90', 'CNDecompCascadeConType.F90', 'landunit_varcon.F90',"LakeCon.F90",
                 'LandunitType.F90', 'ColumnType.F90', 'VegetationType.F90','GridcellType.F90',
                 'TopounitType.F90','CNStateType.F90', 'UrbanParamsType.F90','ColumnDataType.F90',
                 'CH4varcon.F90', 'pftvarcon.F90', 'soilorder_varcon.F90', 'column_varcon.F90',
                 'Tracer_varcon.F90', 'elm_varsur.F90', 'timeinfoMod.F90','VegetationPropertiesType.F90',
                 'domainMod.F90',"decompInitMod.F90", "subgridMod.F90","filterMod.F90"]

preproc_list = ['AllocationMod','dynSubgridControlMod','CH4Mod',
                'GapMortalityMod', 'PhotosynthesisMod', 'SharedParamsMod'
                'PhenologyMod','SnowSnicarMod','NitrifDenitrifMod','SoilLittDecompMod',
                'DecompCascadeBGCMod','DecompCascadeCNMod','SoilLittVertTranspMod'
                ,'SurfaceAlbedoMod','MaintenanceRespMod','SoilWaterMovementMod']

unit_test_files = ["elm_instMod.o","fileio_mod.o","readConstants.o","update_accMod.o",
                   "readMod.o","initializeParameters.o","duplicateMod.o","verificationMod.o",
                   "elm_initializeMod.o","main.o"]

class Colorcodes(object):
    """
    Provides ANSI terminal color codes which are gathered via the ``tput``
    utility. That way, they are portable. If there occurs any error with
    ``tput``, all codes are initialized as an empty string.
    The provides fields are listed below.
    Control:
    - bold
    - reset
    Colors:
    - blue
    - green
    - orange
    - red
    :license: MIT
    """
    def __init__(self):
        import subprocess
        try:
            self.bold = subprocess.check_output("tput bold".split())
            self.reset = subprocess.check_output("tput sgr0".split())

            self.blue = subprocess.check_output("tput setaf 4".split())
            self.green = subprocess.check_output("tput setaf 2".split())
            self.orange = subprocess.check_output("tput setaf 3".split())
            self.red = subprocess.check_output("tput setaf 1".split())
        except subprocess.CalledProcessError as e:
            self.bold = ""
            self.reset = ""

            self.blue = ""
            self.green = ""
            self.orange = ""
            self.red = ""

_c = Colorcodes()

class BColors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

_bc = BColors()