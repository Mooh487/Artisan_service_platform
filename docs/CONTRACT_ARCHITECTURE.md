# Artisan Service Platform - Contract Architecture

This document provides a detailed overview of the smart contract architecture for the Artisan Service Platform.

## Contract Hierarchy

The platform is built on a modular contract architecture with the following components:

```
artisan.clar (Main contract)
├── artisan_service_platform.clar
├── artisan_platform.clar
├── artisan_service.clar
├── booking.clar
├── feedback.clar
├── installment.clar
└── profile.clar
```

Supporting contracts:
- errors.clar
- storage_maps.clar
- types.clar
- minimal.clar (simplified version for testing)

## Data Flow

1. **Registration Flow**:
   - Artisan registers profile (artisan.clar → profile.clar)
   - Profile data stored in maps (storage_maps.clar)

2. **Booking Flow**:
   - Client books artisan (artisan.clar → booking.clar)
   - Payment held in escrow
   - Service completed or disputed
   - Payment released or refunded

3. **Feedback Flow**:
   - Client submits feedback (artisan.clar → feedback.clar)
   - Feedback stored and linked to booking

4. **Installment Flow**:
   - Client initiates installment plan (artisan.clar → installment.clar)
   - Payments tracked and processed

## Contract Details

### artisan.clar

The main contract that orchestrates all platform functionality.

**Key Functions**:
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

**Data Structures**:
- Artisans map: Stores artisan profiles
- Bookings map: Stores booking details
- Escrows map: Manages payment escrows
- Disputes map: Tracks service disputes
- Feedbacks map: Stores client feedback
- Debts map: Manages installment plans

### artisan_service_platform.clar

Handles platform-level functionality.

**Key Functions**:
- `register-artisan`: Register artisan on the platform
- `update-profile`: Update artisan profile
- `book-artisan`: Create a booking
- `cancel-booking`: Cancel a booking
- `submit-feedback`: Submit feedback
- `initiate-installment`: Create installment plan
- `pay-installment`: Process installment payment

### booking.clar

Manages the booking lifecycle.

**Key Functions**:
- `book-artisan`: Create a new booking
- `cancel-booking`: Cancel a booking
- `get-booking`: Retrieve booking details

### feedback.clar

Handles the feedback and rating system.

**Key Functions**:
- `submit-feedback`: Submit feedback for an artisan
- `get-feedback`: Retrieve feedback

### installment.clar

Implements installment payment functionality.

**Key Functions**:
- `initiate-installment`: Create an installment plan
- `pay-installment`: Process an installment payment
- `get-debt`: Retrieve debt details

### profile.clar

Manages artisan profiles.

**Key Functions**:
- `register-artisan`: Create an artisan profile
- `update-profile`: Update profile details
- `get-artisan-profile`: Retrieve profile information

## Security Considerations

1. **Input Validation**:
   - All functions validate input parameters
   - Prevents invalid or malicious data

2. **Access Control**:
   - Functions check caller's identity
   - Only authorized principals can perform actions

3. **Escrow Mechanism**:
   - Payments held in escrow until service completion
   - Protects both artisans and clients

4. **Dispute Resolution**:
   - Admin-controlled dispute resolution
   - Fair mechanism for resolving conflicts

5. **Error Handling**:
   - Standardized error codes (errors.clar)
   - Clear error messages for debugging

## Upgrade Path

The modular design allows for future upgrades:

1. Individual contracts can be upgraded independently
2. New features can be added as separate contracts
3. Core functionality in artisan.clar can be extended

## Testing Strategy

1. Unit tests for individual functions
2. Integration tests for contract interactions
3. Scenario tests for complete user flows
