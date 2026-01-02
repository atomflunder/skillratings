use std::{error::Error, fmt::Display};

use crate::trueskill::TrueSkillRating;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Errors that can occur when passing in invalid weights.
pub enum WeightError {
    /// If the amount of teams does not match.
    TeamAmount,
    /// If the amount of players in a team does not match.
    PlayerAmount {
        /// The team that contains the wrong amount of players.
        team: usize,
    },
    /// If a weight is negative.
    Weight {
        /// The team that contains the wrong weight.
        team: usize,
        /// The player that was assigned the wrong weight.
        player: usize,
    },
}

impl Display for WeightError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TeamAmount => {
                write!(f, "The amount of teams is invalid.")
            }
            Self::PlayerAmount { team } => {
                write!(f, "The amount of players in team {team} is invalid.")
            }
            Self::Weight { team, player } => {
                write!(
                    f,
                    "The weight for player {player} in team {team} is invalid",
                )
            }
        }
    }
}

impl Error for WeightError {}

pub fn get_weights(
    teams: &[&[TrueSkillRating]],
    raw_weights: Option<&[&[f64]]>,
) -> Result<Vec<Vec<f64>>, WeightError> {
    if let Some(weights) = raw_weights {
        if weights.len() != teams.len() {
            return Err(WeightError::TeamAmount);
        }

        for (i, team) in weights.iter().enumerate() {
            if team.len() != teams[i].len() {
                return Err(WeightError::PlayerAmount { team: i });
            }

            for (j, &weight) in team.iter().enumerate() {
                if weight < 0.0 {
                    return Err(WeightError::Weight { team: i, player: j });
                }
            }
        }

        return Ok(weights.iter().map(|team| team.to_vec()).collect());
    }

    Ok(teams.iter().map(|team| vec![1.0; team.len()]).collect())
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use super::*;

    #[test]
    fn invalid_team_amount() {
        let teams: &[&[TrueSkillRating]] = &[&[TrueSkillRating::new()], &[TrueSkillRating::new()]];
        let weights: &[&[f64]] = &[&[0.1], &[0.1], &[0.1]];

        let err = get_weights(teams, Some(weights));
        assert!(matches!(err, Err(WeightError::TeamAmount)));

        let u = err.unwrap_err();
        assert_eq!(
            format!("{:?}", u.to_string()),
            "\"The amount of teams is invalid.\""
        );
    }

    #[test]
    fn invalid_player_amount() {
        let teams: &[&[TrueSkillRating]] = &[&[TrueSkillRating::new()], &[TrueSkillRating::new()]];
        let weights: &[&[f64]] = &[&[0.1], &[0.1, 1.0]];

        let err = get_weights(teams, Some(weights));
        assert!(matches!(err, Err(WeightError::PlayerAmount { team: 1 })));

        let u = err.unwrap_err();
        assert_eq!(
            format!("{:?}", u.to_string()),
            "\"The amount of players in team 1 is invalid.\""
        );
    }

    #[test]
    fn invalid_weight() {
        let teams: &[&[TrueSkillRating]] = &[&[TrueSkillRating::new()], &[TrueSkillRating::new()]];
        let weights: &[&[f64]] = &[&[0.1], &[-1.0]];

        let err = get_weights(teams, Some(weights));
        assert!(matches!(
            err,
            Err(WeightError::Weight { team: 1, player: 0 })
        ));

        let u = err.unwrap_err();
        assert_eq!(
            format!("{:?}", u.to_string()),
            "\"The weight for player 0 in team 1 is invalid\""
        );
    }

    #[test]
    fn valid() {
        let teams: &[&[TrueSkillRating]] = &[&[TrueSkillRating::new()], &[TrueSkillRating::new()]];
        let weights: &[&[f64]] = &[&[0.1], &[1.0]];

        let res = get_weights(teams, Some(weights)).unwrap();
        assert_eq!(res, vec![vec![0.1], vec![1.0]]);
    }

    #[test]
    fn empty() {
        let teams: &[&[TrueSkillRating]] = &[&[TrueSkillRating::new()], &[TrueSkillRating::new()]];

        let res = get_weights(teams, None).unwrap();
        assert_eq!(res, vec![vec![1.0], vec![1.0]]);
    }
}
