use super::TrueSkillRating;

// This Matrix could have been imported, but we implement it ourselves, since we only have to use some basic things here.
#[derive(Clone, Debug)]
pub struct Matrix {
    data: Vec<f64>,
    rows: usize,
    cols: usize,
}

impl Matrix {
    pub fn set(&mut self, row: usize, col: usize, val: f64) {
        self.data[row * self.cols + col] = val;
    }

    pub fn get(&self, row: usize, col: usize) -> f64 {
        self.data[row * self.cols + col]
    }

    pub fn new(rows: usize, cols: usize) -> Self {
        Self {
            data: vec![0.0; rows * cols],
            rows,
            cols,
        }
    }

    pub fn new_from_data(data: &[f64], rows: usize, cols: usize) -> Self {
        Self {
            data: data.to_vec(),
            rows,
            cols,
        }
    }

    pub fn new_diagonal(data: &[f64]) -> Self {
        let mut matrix = Self::new(data.len(), data.len());

        for (i, val) in data.iter().enumerate() {
            matrix.set(i, i, *val);
        }

        matrix
    }

    pub fn create_rotated_a_matrix(teams: &[&[TrueSkillRating]], flattened_weights: &[f64]) -> Self {
        let total_players = teams.iter().map(|team| team.len()).sum::<usize>();

        let mut player_assignments: Vec<f64> = vec![];

        let mut total_previous_players = 0;

        let team_assignments_list_count = teams.len();

        for current_column in 0..team_assignments_list_count - 1 {
            let current_team = teams[current_column];

            player_assignments.append(&mut vec![0.0; total_previous_players]);

            for _current_player in current_team {
                player_assignments.push(flattened_weights[player_assignments.len()]);
                total_previous_players += 1;
            }

            let mut rows_remaining = total_players - total_previous_players;
            let next_team = teams[current_column + 1];

            for _next_player in next_team {
                player_assignments.push(-1.0 * flattened_weights[player_assignments.len()]);
                rows_remaining -= 1;
            }

            player_assignments.append(&mut vec![0.0; rows_remaining]);
        }

        Self::new_from_data(
            &player_assignments,
            team_assignments_list_count - 1,
            total_players,
        )
    }

    pub fn transpose(&self) -> Self {
        let mut matrix = Self::new(self.cols, self.rows);

        for i in 0..self.rows {
            for j in 0..self.cols {
                matrix.set(j, i, self.get(i, j));
            }
        }

        matrix
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub fn determinant(&self) -> f64 {
        assert_eq!(self.rows, self.cols, "Matrix must be square");

        if self.rows == 1 {
            return self.get(0, 0);
        }

        let mut sum = 0.0;

        for i in 0..self.rows {
            sum += self.get(0, i) * self.minor(0, i).determinant() * (-1.0_f64).powi(i as i32);
        }

        sum
    }

    pub fn minor(&self, row: usize, col: usize) -> Self {
        let mut matrix = Self::new(self.rows - 1, self.cols - 1);

        for i in 0..self.rows {
            for j in 0..self.cols {
                if i != row && j != col {
                    matrix.set(
                        if i > row { i - 1 } else { i },
                        if j > col { j - 1 } else { j },
                        self.get(i, j),
                    );
                }
            }
        }

        matrix
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub fn adjugate(&self) -> Self {
        let mut matrix = Self::new(self.rows, self.cols);

        for i in 0..self.rows {
            for j in 0..self.cols {
                matrix.set(
                    i,
                    j,
                    self.minor(j, i).determinant() * (-1.0_f64).powi((i + j) as i32),
                );
            }
        }

        matrix
    }

    pub fn inverse(&self) -> Self {
        let det = self.determinant();

        // Avoiding 1/0
        assert!((det != 0.0), "Matrix is not invertible");

        self.adjugate() * det.recip()
    }
}

impl std::ops::Mul for Matrix {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.cols == rhs.rows {
            let mut matrix = Self::new(self.rows, rhs.cols);

            for i in 0..self.rows {
                for j in 0..rhs.cols {
                    let mut sum = 0.0;

                    for k in 0..self.cols {
                        sum += self.get(i, k) * rhs.get(k, j);
                    }

                    matrix.set(i, j, sum);
                }
            }

            matrix
        } else if self.rows == rhs.cols {
            let mut matrix = Self::new(self.cols, rhs.rows);

            for i in 0..self.cols {
                for j in 0..rhs.rows {
                    let mut sum = 0.0;

                    for k in 0..self.rows {
                        sum += self.get(k, i) * rhs.get(j, k);
                    }

                    matrix.set(i, j, sum);
                }
            }

            matrix
        } else {
            panic!("Cannot multiply matrices with incompatible dimensions");
        }
    }
}

impl std::ops::Mul<f64> for Matrix {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        let mut matrix = Self::new(self.rows, self.cols);

        for i in 0..self.rows {
            for j in 0..self.cols {
                matrix.set(i, j, self.get(i, j) * rhs);
            }
        }

        matrix
    }
}

impl std::ops::Add for Matrix {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        assert_eq!(
            self.rows, rhs.rows,
            "Cannot add matrices with different row counts"
        );
        assert_eq!(
            self.cols, rhs.cols,
            "Cannot add matrices with different column counts"
        );

        let mut matrix = Self::new(self.rows, self.cols);

        for i in 0..self.rows {
            for j in 0..self.cols {
                matrix.set(i, j, self.get(i, j) + rhs.get(i, j));
            }
        }

        matrix
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matrix_panics() {
        use std::panic::catch_unwind;

        let result = catch_unwind(|| Matrix::new(2, 3).determinant());
        assert!(result.is_err());

        let result = catch_unwind(|| Matrix::new(2, 2).inverse());
        assert!(result.is_err());

        let result = catch_unwind(|| Matrix::new(2, 2) * Matrix::new(3, 3));
        assert!(result.is_err());

        let result = catch_unwind(|| Matrix::new(3, 2) + Matrix::new(2, 2));
        assert!(result.is_err());

        let result = catch_unwind(|| Matrix::new(2, 2) + Matrix::new(2, 3));
        assert!(result.is_err());
    }

    #[test]
    fn test_misc() {
        assert!(!format!("{:?}", Matrix::new(2, 3)).is_empty());
    }
}
