# Common
target_sources(
                util_common
                PRIVATE
                  read_met_module.F
                  write_met_module.F
                  module_debug.F
                  constants_module.F
                  met_data_module.F
                  cio.c
                  misc_definitions_module.F
                  gridinfo_module.F
                  module_date_pack.F
                )

target_sources(
                rd_intermediate
                PRIVATE
                  rd_intermediate.F
              )

# target_sources(
#                 plotgrids
#                 PRIVATE
#                   plotgrids.F
#                   module_map_utils.F
#               )

target_sources(
                avg_tsfc
                PRIVATE
                  avg_tsfc.F
              )

# target_sources(
#                 elev_angle
#                 PRIVATE
#                   elev_angle.F
#               )

target_sources(
                calc_ecmwf_p
                PRIVATE
                  calc_ecmwf_p.F
                  module_stringutil.F
              )


# target_sources(
#                 plotfmt
#                 PRIVATE
#                   plotfmt.F
#               )

target_sources(
                mod_levs
                PRIVATE
                  mod_levs.F
              )
if ( DEFINED WRF_DIR )
  target_sources(
                  int2nc
                  PRIVATE
                    int2nc.F
                )
endif()

target_sources(
                height_ukmo
                PRIVATE
                  height_ukmo.F
              )