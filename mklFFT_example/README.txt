˵��:

shuju.dat�е�������sin(2*pi*f1*t) + sin(2*pi*f2*t)��á�f1 = 1kHz, f2 = 3kHz
�������IDE(VS+IVF)�����д˳���Ҫ����mkl�����⡣


�����win�������������д˳���
��Ӧ������Ϊ: ifort /Qmkl mkl_dfti.f90 FFT.f90



�����linux�������������г���
��Ӧ������Ϊ: ifort -mkl mkl_dfti.f90 FFT.f90

