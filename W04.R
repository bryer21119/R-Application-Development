# Initialize bank account with balance and transaction history
bank_account <- list(
  account_number = "123456789",
  balance = 1000,  # Initial balance
  transactions = list()
)

# Function to deposit money into the account
deposit <- function(amount) {
  if (amount > 0) {
    bank_account$balance <- bank_account$balance + amount
    transaction <- list(type = "Deposit", amount = amount)
    bank_account$transactions <- c(bank_account$transactions, list(transaction))
    cat("Deposit successful. Current balance:", bank_account$balance, "\n")
  } else {
    cat("Invalid amount for deposit. Please enter a positive amount.\n")
  }
}

# Function to withdraw money from the account
withdraw <- function(amount) {
  if (amount > 0 && amount <= bank_account$balance) {
    bank_account$balance <- bank_account$balance - amount
    transaction <- list(type = "Withdrawal", amount = amount)
    bank_account$transactions <- c(bank_account$transactions, list(transaction))
    cat("Withdrawal successful. Current balance:", bank_account$balance, "\n")
  } else {
    cat("Invalid amount for withdrawal or insufficient funds.\n")
  }
}

# Function to display current balance
display_balance <- function() {
  cat("Current balance:", bank_account$balance, "\n")
}

# Function to show transaction history
show_transactions <- function() {
  cat("Transaction History:\n")
  for (transaction in bank_account$transactions) {
    cat("Type:", transaction$type, "- Amount:", transaction$amount, "\n")
  }
}

# Function to transfer funds between accounts
transfer <- function(amount, target_account) {
  if (amount > 0 && amount <= bank_account$balance) {
    bank_account$balance <- bank_account$balance - amount
    target_account$balance <- target_account$balance + amount
    transaction <- list(type = "Transfer to " + target_account$account_number, amount = amount)
    bank_account$transactions <- c(bank_account$transactions, list(transaction))
    cat("Transfer successful. Current balance:", bank_account$balance, "\n")
  } else {
    cat("Invalid amount for transfer or insufficient funds.\n")
  }
}

# Function to calculate and add interest
add_interest <- function(rate) {
  interest <- bank_account$balance * (rate / 100)
  bank_account$balance <- bank_account$balance + interest
  transaction <- list(type = "Interest", amount = interest)
  bank_account$transactions <- c(bank_account$transactions, list(transaction))
  cat("Interest added. Current balance:", bank_account$balance, "\n")
}

# Example usage
deposit(500)
withdraw(200)
display_balance()

# Create another bank account
another_account <- list(
  account_number = "987654321",
  balance = 2000,
  transactions = list()
)

# Transfer funds from one account to another
transfer(300, another_account)

# Show transaction history
show_transactions()

# Add interest to the account
add_interest(5)
display_balance()
