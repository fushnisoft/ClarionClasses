! See zd_alloc.clw for full documentation
!First create a dummy ZD.clw file that contains this:
  PROGRAM
  MAP
  END
  CODE
!This is a trick to stop names leaking from here into your main app and potentially
!causing conflicts. You do *NOT* need to add ZD.clw to your project.