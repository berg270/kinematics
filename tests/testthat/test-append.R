context('Append dynamics')

test_that('With displacement',
          {
            # Generate movement with uniform speed
            r_0 <- c(0, 1) # Initial positions
            v_0 <- c(1.2, 0.8) # Initial (and constant) speed
            t <- seq(0, 1, by=0.05)

            mov <- uniformly_accelerated_mov(t, r_0, v_0)

            # Append dynamical information
            mov <- append_dynamics(mov, append.displacement = TRUE)

            # Check that the information is correct
            expect_true(all(c('vx', 'vy', 'aspeed') %in% colnames(mov)), 'Some speeds have not been calculated')
            expect_true(all(c('ax', 'ay', 'aaccel') %in% colnames(mov)), 'Some accelerations have not been calculated')
            expect_true(all(c('curv', 'curv_radius') %in% colnames(mov)), 'Some curvatures have not been calculated')
            expect_true(all(c('disp_x', 'disp_y', 'adisp') %in% colnames(mov)), 'The displacements have not been calculated')
          }
)

test_that('Without displacement',
          {
            # Generate movement with uniform speed
            r_0 <- c(0, 1) # Initial positions
            v_0 <- c(1.2, 0.8) # Initial (and constant) speed
            t <- seq(0, 1, by=0.05)

            mov <- uniformly_accelerated_mov(t, r_0, v_0)

            # Append dynamical information
            mov <- append_dynamics(mov, append.displacement = FALSE)

            # Check that the information is correct
            expect_true(all(c('vx', 'vy', 'aspeed') %in% colnames(mov)), 'Some speeds have not been calculated')
            expect_true(all(c('ax', 'ay', 'aaccel') %in% colnames(mov)), 'Some accelerations have not been calculated')
            expect_true(all(c('curv', 'curv_radius') %in% colnames(mov)), 'Some curvatures have not been calculated')

            expect_false(any(c('disp_x', 'disp_y', 'adisp') %in% colnames(mov)), 'The displacement is not supposed to be calculated')
          }
)
