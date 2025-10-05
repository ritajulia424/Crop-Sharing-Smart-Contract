# 🌾 Crop Sharing Smart Contract

A decentralized platform for splitting harvest revenue among farmers, investors, and cooperatives built on the Stacks blockchain using Clarity smart contracts.

## 🚀 Features

- **🌱 Crop Creation**: Farmers can create crop projects with customizable revenue sharing percentages
- **💰 Investment System**: Investors can fund crop projects and earn proportional returns
- **🤝 Cooperative Integration**: Co-ops can provide services and earn their share of harvest revenue
- **📊 Transparent Revenue Distribution**: Automated splitting of harvest revenue based on predefined percentages
- **🔍 Real-time Tracking**: Monitor investments, earnings, and crop status

## 📋 Contract Functions

### Core Functions

#### `create-crop`
Creates a new crop project with specified revenue sharing percentages.
```clarity
(create-crop "Corn" u40 u50 u10) ;; 40% farmer, 50% investors, 10% coop
```

#### `invest-in-crop`
Allows investors to fund a crop project.
```clarity
(invest-in-crop u1 u1000000) ;; Invest 1 STX in crop ID 1
```

#### `register-coop-service`
Registers a cooperative service for a crop project.
```clarity
(register-coop-service u1 u500000) ;; Register service for crop ID 1
```

#### `record-harvest`
Records harvest revenue (contract owner only).
```clarity
(record-harvest u1 u5000000) ;; Record 5 STX harvest revenue
```

### Claiming Functions

#### `claim-farmer-share`
Farmers claim their percentage of harvest revenue.

#### `claim-investor-share`
Investors claim their proportional share based on investment amount.

#### `claim-coop-share`
Cooperatives claim their service fee percentage.

### Read-Only Functions

#### `get-crop`
Retrieves complete crop information.

#### `get-user-investments`
Shows user's total investments and active crops.

#### `get-user-earnings`
Displays user's earnings history.

#### `calculate-potential-returns`
Calculates potential returns for an investor.

#### `get-crop-summary`
Provides a summary of crop financials.

## 🛠️ Usage Instructions

### 1. Deploy the Contract
```bash
clarinet deploy
```

### 2. Create a Crop Project
The contract owner creates a new crop with revenue sharing percentages:
```bash
clarinet console
```
```clarity
(contract-call? .crop-sharing create-crop "Wheat" u45 u45 u10)
```

### 3. Investment Phase
Investors can fund the crop project:
```clarity
(contract-call? .crop-sharing invest-in-crop u1 u2000000)
```

### 4. Cooperative Services
Co-ops register their services:
```clarity
(contract-call? .crop-sharing register-coop-service u1 u300000)
```

### 5. Harvest Recording
After harvest, record the revenue:
```clarity
(contract-call? .crop-sharing record-harvest u1 u8000000)
```

### 6. Claim Rewards
Each participant claims their share:
````clarity
(contract-call? .crop-sharing claim-farmer-share u1)
(contract-call? .crop-sharing claim-investor-share u1)
(contract-call?
