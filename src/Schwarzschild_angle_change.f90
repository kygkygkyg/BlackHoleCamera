program main
    implicit none
    double precision, parameter :: PI = acos(-1.0d0)
    integer, parameter :: m = 1000 ! ピクセル指定
    double precision, parameter :: r_bh = 1.0d0, delta_b = 0.010d0, d_theta = 0.01d0, &
                                &  ymax = 10.0d0, xmax = 10.0d0, delta_phi = 2.0d0*PI/dble(m - 1), &
                                &  b_cut = 0.50d0, angle = (5.0d0)*PI/180.0d0

    double precision :: w, u, theta, dw, du
    double precision :: b,  x, intencity(1:m), intencity_first, &
                      & intencity_stock(1:m)
    ! bは光線発射位置のパラメータ, intencityは光の増幅された強度
    ! tauは光学的厚み, kaiは吸収係数, dlは微笑変化距離
    double precision :: xp, yp
  
  !　各物理量の定数----------------------------------------------------------!
  double precision, parameter :: c = 1.0d0, h = 1.0d0, k = 1.0d0, nu = 1.0d0 !
  !光速度, プランク定数, ボルツマン定数, 輻射の振動数                          !
  !-------------------------------------------------------------------------!
  ! 初期値設定---------------------------------------------------!
  b = 15.0d0                                                    !
  xp = 0.0d0 ; yp = 0.0d0 ; x = 10.0d0 ; intencity_first = 1.0d0 !
  !--------------------------------------------------------------!
  
  open(101, file = 'Schwarzschild_LightPath.dat', status = 'unknown') ! 軌跡の出力先
  open(102, file = 'Black_Hall_camera.dat', status = 'unknown') ! ブラックホールカメラの出力先
  open(103, file = 'Light_intencity.dat', status = 'unknown') ! 強度のｂ依存
  
  loop_b : do ! bが０になるまでのループ
  
    !　初期条件
    theta = atan(b/x)
    u = 1.0d0/sqrt(x*x + b*b)
    w = -sqrt(1.0d0-(r_bh*u))*u/tan(PI-theta)
    intencity = 1.0d0
  
    !　計算開始
    loop_r : do !ｒが事象の地平線にはいるまで計算

      write(101,'(3f15.8)') theta, u, 1.0d0/u ! uは距離ｒの逆数

      call runge_kutta(theta,u,w,du,dw)
      u = u + du
      w = w + dw
      theta = theta + d_theta

      if(u .GT. 1.0d0/r_bh) exit
      if(u .LT. 5.0d-2/r_bh) exit

      if(b .GT. b_cut) then
      call Radiative_Transfer(u,theta,intencity) !ステップごとに輻射輸送方程式で強度の更新
      end if

    end do loop_r

    !　カメラのファイルへの出力
    call write_SphereSymmetry(b,intencity)

    b = b - delta_b
    if(b .LT. 0.0d0) exit

  end do loop_b
  
  close(101)
  close(102)
  close(103)


  

  !-------------------------------------------------------------------------------------------------!
  !-------------------------------------------SUBROUTINE--------------------------------------------!
  !-------------------------------------------------------------------------------------------------!
  CONTAINS

    subroutine dw_dtheta(u,w_f)  ! ----------------------------------------------ｗのほうの微分方程式
      implicit none
      double precision, intent(in) :: u
      double precision, intent(out) :: w_f
  
      w_f = 1.5d0*r_bh*u*u - u
  
      return
  
    end subroutine dw_dtheta

    subroutine runge_kutta(theta,u,w,du,dw) ! -----------------------------ルンゲクッタ法のサブルーチン
      implicit none
      double precision, intent(in) :: theta, u, w
      double precision, intent(out) :: dw, du
      double precision :: k1, k2, k3, k4, h1, h2, h3, h4

      h1 = w*d_theta
      call dw_dtheta(u,k1)
      h2 = w + 0.5d0*h1*d_theta
      call dw_dtheta(u+0.5d0*h1,k2)
      h3 = w + 0.5d0*h2*d_theta
      call dw_dtheta(u+0.5d0*h2,k3)
      h4 = w + h3*d_theta
      call dw_dtheta(u+h3,k4)

      du = (h1 + 2.0d0*h2 + 2.0d0*h3 + h4)*d_theta/6.0d0
      dw = (k1 + 2.0d0*k2 + 2.0d0*k3 + k4)*d_theta/6.0d0

      return

    end subroutine runge_kutta

    subroutine absorption(r,theta,phi,kai) !------------------------------------------- 吸収係数の関数
      implicit none
      double precision, intent(in) :: r, theta, phi
      double precision, intent(out) :: kai
      double precision :: x_prime, y_prime, z_prime, r_col

      ! if(r .LT. 10.0d0)then
      !   kai = 0.20d0 ! exp(-1.0d0/(u*u))
      ! else
      !   kai = 0.0d0
      ! end if

      ! if( abs( r*sin(theta) ) .LT. 10.0d0 .AND. abs( r*cos(theta) ) .LT. 0.1d0) then
      !   kai = 0.20d0
      ! else
      !   kai = 0.0d0
      ! end if

      x_prime = -r*sin(theta)*cos(phi)
      y_prime =  r*cos(theta)*cos(angle) - r*sin(theta)*sin(phi)*sin(angle)
      z_prime =  r*cos(theta)*sin(angle) + r*sin(theta)*sin(phi)*cos(angle)

      r_col = sqrt( x_prime*x_prime + y_prime*y_prime )

      if(r_col .LE. 10.0d0 .AND. abs(z_prime) .LE. 1.0d0)then
        kai = 1.0d0 ! exp(-1.0d0/(u*u))
      else
        kai = 0.0d0
      end if

    end subroutine absorption

    subroutine dencity(u,n) !------------------------------------------------------ガス密度(球対称)
      implicit none
      double precision, intent(in) :: u
      double precision, intent(out) :: n

      n = u

    end subroutine dencity

    subroutine Source_func(u,S) !----------------------------------------------------------光源関数
      double precision, intent(in) :: u
      double precision :: S, n

      call dencity(u,n) !　理想気体とすると温度は密度に反比例
      S = 2.0d0  !  *h*(nu*nu*nu/c*c*c)/(exp(h*nu*n/k) - 1.0d0)

    end subroutine Source_func

    subroutine Radiative_Transfer(u,theta,intencity) ! -------------------------輻射輸送の時の積分
      implicit none
      integer :: i
      double precision, intent(in) :: u, theta
      double precision :: phi, intencity(1:m), S, dl, dr, r, kai, delta_tau 
      !intencityは光線の強度, Sは光源関数

      phi = 0.0d0

      call Source_func(u,S)
      do i = 1,m

        r  = 1.0d0 / u
        dr = -du / (u*u)
        dl = sqrt(dr*dr*r/(r - 1.0d0) + d_theta*d_theta*r*r)   !　dlは測地線の微小変化距離
        call absorption(r,theta,phi,kai) ! 吸収係数を求めている
        delta_tau = kai*dl ! 光学的厚みの計算

        intencity(i) = intencity(i) + exp(-delta_tau)*S*delta_tau
        phi = phi + delta_phi
      end do
    
    end subroutine Radiative_Transfer

    subroutine write_SphereSymmetry(b,intencity) !-----------------------------------ファイルの出力
      implicit none
      integer :: i
      double precision :: b, intencity(1:m)
      double precision :: phi

      if(b .GT. b_cut) then
        intencity_stock = intencity
      else
        intencity = intencity_stock
      end if
      !---------------------------------------------------------!
      ! write(103,'(2f15.8)') b, log(intencity)                        !
      ! write(103,'(2f15.8)') -b, log(intencity)                       !
       write(103,'(2f15.8)')  b, intencity(1)                       !
       write(103,'(2f15.8)') -b, intencity(1)                       !
                                                                !
      phi = 0.0d0                                            ! phiは回転させたときの位相 
      
      do i = 1,m
        if(b .GT. b_cut) then
          intencity_stock(i) = intencity(i)
        else
          intencity(i) = intencity_stock(i)
        end if                                              !

        write(102,'(3f15.8)') phi, intencity(i), b  !
        phi = phi + delta_phi                                      ! 0.01はphiの刻み幅                             !
      end do

      write(102,*) ''                                         !
      !---------------------------------------------------------!

  end subroutine write_SphereSymmetry

  
  end program main