# Artisan Service Platform

A decentralized platform for artisans to offer services, clients to book services, and manage the entire service lifecycle using Clarity smart contracts on the Stacks blockchain.

## Overview

The Artisan Service Platform is a comprehensive solution for connecting skilled artisans with clients seeking their services. The platform handles:

- Artisan registration and profile management
- Service booking and payment processing
- Escrow services for secure transactions
- Dispute resolution mechanisms
- Feedback and rating system
- Installment payment options

All functionality is implemented using Clarity smart contracts on the Stacks blockchain, ensuring transparency, security, and immutability.

## Smart Contracts

The platform consists of several interconnected smart contracts:

### Core Contracts

- **artisan.clar**: Main contract handling artisan registration, booking, payments, and dispute resolution
- **artisan_service_platform.clar**: Platform-level functionality for managing artisans and services
- **artisan_platform.clar**: Handles artisan profiles and platform interactions
- **artisan_service.clar**: Manages service offerings and bookings

### Supporting Contracts

- **booking.clar**: Handles the booking lifecycle
- **feedback.clar**: Manages the feedback and rating system
- **installment.clar**: Implements installment payment functionality
- **profile.clar**: Manages artisan profiles
- **minimal.clar**: Simplified version of the platform for testing
- **errors.clar**: Defines error codes used across contracts
- **storage_maps.clar**: Defines data storage structures
- **types.clar**: Defines custom types used in the contracts

## Features

### For Artisans

- Create and manage professional profiles
- Offer services with detailed descriptions
- Accept or decline booking requests
- Receive secure payments through escrow
- Build reputation through client feedback

### For Clients

- Browse artisan profiles and services
- Book services with secure payment handling
- Pay in full or through installment plans
- Provide feedback and ratings for completed services
- Raise disputes for unsatisfactory services

### Platform Features

- Secure escrow payment system
- Dispute resolution mechanism
- Installment payment options
- Comprehensive feedback system
- Platform fee management

## Technical Details

### Contract Architecture

The platform uses a modular contract architecture:

1. **Core Functionality**: Implemented in the main artisan contract
2. **Data Storage**: Managed through maps and variables
3. **Access Control**: Principal-based permissions
4. **Error Handling**: Standardized error codes

### Key Functions

- `register-artisan`: Register a new artisan profile
- `book-artisan`: Book a service from an artisan
- `cancel-booking`: Cancel a pending booking
- `complete-booking`: Mark a booking as completed
- `confirm-completion`: Client confirms service completion
- `create-dispute`: Raise a dispute for a booking
- `resolve-dispute`: Resolve a dispute (admin only)
- `submit-feedback`: Submit feedback for a completed service
- `initiate-installment`: Set up an installment payment plan
- `pay-installment`: Make an installment payment

## Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) - Clarity development environment
- [Stacks CLI](https://github.com/blockstack/stacks.js) - For interacting with the Stacks blockchain

### Installation

1. Clone the repository:
   ```
   git clone https://github.com/Mooh487/Artisan_service_platform.git
   cd artisan_service_platform
   ```

2. Install dependencies:
   ```
   npm install
   ```

3. Run Clarinet check to verify contracts:
   ```
   clarinet check
   ```

### Testing

Run the test suite to verify contract functionality:

```
clarinet console
```

## Deployment

### Testnet Deployment

1. Configure your Stacks wallet:
   ```
   stacks config setup
   ```

2. Deploy to testnet:
   ```
   clarinet deployments apply --testnet
   ```

### Mainnet Deployment

1. Ensure all tests pass and contracts are audited
2. Deploy to mainnet:
   ```
   clarinet deployments apply --mainnet
   ```

## Security Considerations

- All contracts include comprehensive input validation
- Escrow mechanism ensures secure payments
- Admin-controlled dispute resolution
- Principal-based access control

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Stacks Foundation for the blockchain infrastructure
- Clarity language developers
- The open-source community for valuable tools and libraries
