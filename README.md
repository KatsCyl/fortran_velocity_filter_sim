fortran_velocity_filter_sim
===========================

Mandatory university project for FORTAN course

Usage:
  1. Compile with "gfortran particleDef_module.f95 io_module.f95 simulation_module.f95 tester.f95"
  2. Run with "./a.out <timestep> <bx> <by> <bz> <ex> <ey> <ez> <input filename>"
  3. Read output from output.dat

What does it do?
  This program is a simple Wenn filter simulator, it simulates particles going through a uniform 
magnetic and electric fields inside a box. The particles which got through the box are printed in
output.dat. The box's dimensions are defaulted Lx=Lz=1.9cm and Ly=7.6cm.

Assignment of the project can be found at:https://moodle.helsinki.fi/pluginfile.php/869615/mod_assign/intro/final_project.pdf
Guest login works
