Module MKL_FFT   !.. 部分代码参考http://bbs.fcode.cn/thread-1128-1-1.html
  use MKL_DFTI
  private
  Type , public :: CLS_FFT
    type(DFTI_DESCRIPTOR), Pointer :: h => NULL()
    Integer :: Err
  contains
    Procedure :: Create
    Procedure :: Forward
    Procedure :: Backward
    Procedure :: Destory
  End Type CLS_FFT

contains

  Subroutine Create( this , N )
    class( CLS_FFT ) :: this
    Integer , Intent( IN ) :: N
    this%Err = DftiCreateDescriptor( this%h , DFTI_SINGLE , DFTI_REAL , 1 , N )
    this%Err = DftiSetValue( this%h , DFTI_PLACEMENT , DFTI_NOT_INPLACE )
    this%Err = DftiCommitDescriptor( this%h )
  End Subroutine Create

  Function Forward( this , X ) result( F )
    class( CLS_FFT ) :: this
    Real :: X(:)
    Complex :: F( size(X)/2+1 )
    this%Err = DftiComputeForward( this%h , X , F )
  End Function Forward

  Function Backward( this , X ) result( T )
    class( CLS_FFT ) :: this
    Complex :: X(:)
    Real    :: T( size(X)*2-1 )
    this%Err = DftiComputeBackward( this%h , X , T )
  End Function Backward

  Subroutine Destory( this )
    class( CLS_FFT ) :: this
    this%Err = DftiFreeDescriptor( this%h )
  End Subroutine Destory

End Module MKL_FFT

Program mklFFT  
  use MKL_FFT   
  Type( CLS_FFT ) :: FFT 
  Integer :: i, fileid
  Integer :: N !//不需要2^k次方整幂
  Real, allocatable :: t(:), r(:)  !// 时间域
  Complex, allocatable  :: f(:) !// 频率域，为复数,大小为 n/2 + 1
  character(len=215) :: filename = 'shuju.dat'

  call GetN( N, filename )
  allocate( t(N), r(N), f(N/2+1) )
  
  open( newunit = fileid, file = trim(filename) )  !// 读取原始信号
  Do i = 1, N
    read( fileid, * ) t(i), r(i)
  End do
  close( fileid )
  
  call FFT%Create( N ) !// 创建FFT过程。N 是 FFT 的点数 ************
  f  = FFT%Forward( r )  !// 正向 FFT 变换  *************
  call output_BWDFFT( t, f, N )

  r = FFT%Backward( f ) / n !// 这一句可以做反傅氏变换 *************
  call Output_InvFFT( t, r, N )
  call FFT%Destory() !// 销毁 FFT 过程*************
  
  deallocate( t, r, f )
  
End Program mklFFT
  
  
Subroutine GetN( N, filename )  !// 得到信号文件的行数
  Implicit none
  Integer :: N
  character(*), intent(in) :: filename
  integer :: fileid, info
  
  open( newunit = fileid, file = trim(filename) )
  N = 0
  Do 
    read( fileid, *, iostat = info )
    if ( info /= 0 ) exit
    N = N + 1
  End do
  close( fileid )
  
End subroutine GetN
    
Subroutine output_BWDFFT( t, f, N )  !// 输出FFT的频谱数据
  implicit none
  Integer :: i, fileid
  Integer, intent(in) :: N
  Real, intent(in) :: t(N)
  Complex :: f(N/2+1)
  Real :: Fs  !// 采样频率: 一秒钟样点数目
  Real :: t1, t2, w, w0  !// w0为频率间隔，即基频
  
  t1 = t(1); t2 = t(N)
  Fs = dble(N) / ( t2 - t1 )
  w0 = Fs / dble(N)  !// 或者w0 = 1.d0 / ( t2 - t1 )
  
  open( newunit = fileid, file = 'FwdFFT.dat' )
  Do i = 1, N/2+1
    w = dble(i) * w0
    write( fileid, * ) w, sqrt( real(f(i))**2 + imag(f(i))**2 ), atan( imag(f(i)) / real(f(i)) )
  End do
  close( fileid )
  
End subroutine Output_BWDFFT
  
Subroutine output_InvFFT( t, r, N )  !// 输出FFT反变换数据
  implicit none
  Integer :: i, fileid
  Integer, intent(in) :: N
  Real, intent(in) :: t(N), r(N)
  
  open( newunit = fileid, file = 'InvFFT.dat' )
  Do i = 1, N
    write( fileid, * ) t(i), r(i)
  End do
  close( fileid )
  
End subroutine Output_InvFFT