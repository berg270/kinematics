context('Kinematics')

test_that('Speed',
          {
            # Generate movement with uniform speed
            r_0 <- c(0, 1) # Initial positions
            v_expected <- c(1.2, 0.8) # Initial (and constant) speed
            t <- seq(0, 1, by=0.05)

            mov <- uniformly_accelerated_mov(t, r_0, v_expected)

            # Estimate the speeds from the generated data
            vs_estimated <- speed(t, mov$x, mov$y)

            # Check that the estimates are correct
            expect_equal_constvect(v_expected[1], vs_estimated$vx)
            expect_equal_constvect(v_expected[2], vs_estimated$vy)
          }
)

test_that('Acceleration',
          {
            # Generate movement with uniform acceleration
            r_0 <- c(0, 1) # Initial positions
            v_0 <- c(1.2, 0.8) # Initial speed
            a_expected <- c(0.0, -9.8) # Initial (and constant) acceleration
            t <- seq(0, 1, by=0.05)

            mov <- uniformly_accelerated_mov(t, r_0, v_0, a_expected)

            # Estimate the speeds from the generated data
            vs_estimated <- speed(t, mov$x, mov$y)
            as_estimated <- accel(t, mov$x, mov$y)

            # Check that the estimates are correct
            expect_equal_constvect(a_expected[1], as_estimated$ax)
            expect_equal_constvect(a_expected[2], as_estimated$ay)

            # The speeds should not be constant
            expect_true(max(abs(vs_estimated$vx - v_0[1])) > 0.0)
            expect_true(max(abs(vs_estimated$vy - v_0[2])) > 0.0)
          }
)

test_that('Curvature (circle)',
          {
            # Generate circular movement of radius 2
            R_expected <- 2
            t <- seq(0, 2*pi, by=0.05)
            x <- R_expected*cos(t)
            y <- R_expected*sin(t)

            # Estimate the speeds from the generated data
            curvs_estimated <- curvature_radius(t, x, y)

            # Check that the estimates are correct
            tol <- 1e-3
            expect_equal_constvect(R_expected, curvs_estimated, tolerance = tol)
          }
)

test_that('Curvature (ellipse)',
          {
            # Generate an elliptic movement with parameters a and b
            a <- 2
            b <- 3

            t <- seq(0, 2*pi, by=0.05)
            x <- a*cos(t)
            y <- b*sin(t)

            # The local curvature radii can be calculated analytically
            R_expected <- (a^2*(sin(t))^2 + b^2*(cos(t))^2)^(3/2)/(a*b)

            # Estimate the speeds from the generated data
            curvs_estimated <- curvature_radius(t, x, y)

            # Check that the estimates are correct
            tol <- 1e-3
            expect_equal(R_expected, curvs_estimated, tolerance = tol)
          }
)

test_that('Displacement',
          {
            # Generate movement with uniform speed
            r_0 <- c(0, 1) # Initial positions
            v_0 <- c(1.2, 0.8) # Initial (and constant) speed
            t <- seq(0, 1, by=0.05)

            mov <- uniformly_accelerated_mov(t, r_0, v_0)

            # Estimate the displacements from the generated data
            disps <- displacement(mov$x, mov$y)

            # Check that the first row contains only NAs
            # There, the displacements don't make any sense (displacement
            # between when and when?)
            expect_nas(disps[1, ], "Displacement shouldn't be defined for the first row")

            # Check that the remaining rows contain correct estimates
            expect_equal_constvect(0.05 * v_0[1], disps[-1, ]$disp_x)
            expect_equal_constvect(0.05 * v_0[2], disps[-1, ]$disp_y)
          }
)
